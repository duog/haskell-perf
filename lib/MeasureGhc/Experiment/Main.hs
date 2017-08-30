{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DuplicateRecordFields, OverloadedLabels, DataKinds #-}
module MeasureGhc.Experiment.Main where

import System.FilePath
import System.Directory
import System.Process
import System.IO.Temp
import System.Environment
import Control.Concurrent.QSemN
import Data.Generics.Labels()
import Data.Generics.Product
import Data.Time.Clock
import Data.Time.Format
import Stackage.Types (BuildPlan)

import MeasureGhc.Schema.Types
import MeasureGhc.Experiment.BuildPlan
import Distribution.Types.PackageName

import MeasureGhc.Prelude
import qualified MeasureGhc.Text as T
import qualified MeasureGhc.JSON as J
import qualified MeasureGhc.ByteString as BS

data GhcVersion = Ghc800 | Ghc821 | GhcHead


data Options = Options
  { workDir :: FilePath
  , bindistDir :: FilePath
  , packages :: [Text]
  , ghcs :: Map Text GhcSpec
  , ghcsOptions :: Map Text [Text]
  , buildPlanFile :: FilePath
  , dryRun :: Bool
  , jobs :: Int
  } deriving (Generic)
-- makeLensesWith underscoreFields ''Options

defaultOptions :: MonadIO m => m Options
defaultOptions = do
  pwd <- liftIO getCurrentDirectory
  let workDir = pwd </> "work"
      ghcs = _Wrapped #
        [ ("8.2.1", GhcBindist "8.2.1" "ghc-8.2.1" "ghc-8.2.1-x86_64-deb8-linux.tar.xz")
        , ("doug", GhcBindist "8.3-doug" "doug exp 1" "ghc-8.3.20170813-x86_64-unknown-linux.tar.xz")
        ]
      ghcsOptions = _Wrapped #
        [ ("stock", ["-j4"])
        ]
      packages = ["exceptions", "profunctors"]
      buildPlanFile = pwd </> "measure-ghc/nightly-2017-08-15.yaml"
      dryRun = False
      jobs = 1
      bindistDir = pwd </> "bindists"
  return Options{..}

-- data RunDetails = RunDetails
--   { _rd_package :: Text
--   , _rd_ghc :: (FilePath, FilePath)
--   , _rd_ghcOptions :: [Text]
--   , _rd_statsFile :: FilePath
--   , _rd_statsFileLock :: FilePath
--   , _rd_shimSettingsFile :: FilePath
--   , _rd_label :: Text
--   } deriving (Generic, J.FromJSON, J.ToJSON)

mkLabel :: Text -> Text -> Text
mkLabel ghc ghcOptions = ghc <> "-" <> ghcOptions

try_ :: MonadMask m => m () -> m ()
try_ = fmap (either (const ()) id) . tryAny

installGhc :: FilePath -> FilePath -> FilePath -> GhcSpec -> IO ()
installGhc from_dir to_dir temp_dir GhcBindist{..} = do
  putStrLn $ "Installing: " <> T.unpack bindistDescription
  withTempDirectory temp_dir "ghc-extract" $ \extract_dir -> do
    putStrLn $ "Unpacking..."
    callProcess "tar" ["xf", from_dir </> bindistLocation, "-C", extract_dir]
    withCurrentDirectory extract_dir $ do
      inner_dir <- listDirectory "." >>= \case
        [x] -> return x
        xs -> fail $ "Expected single directory inside '" <> bindistLocation <> "' but found:" <> show xs
      withCurrentDirectory inner_dir $ do
        putStrLn "Configuring..."
        callProcess "./configure" ["--prefix=" <> (to_dir </> T.unpack bindistLabel)]
        putStrLn "Installing..."
        callProcess "make" ["install"]
        putStrLn "Done."

main :: IO ()
main = do
  putStrLn "sharni rules"
  Options{..} <- defaultOptions
  unsetEnv "GHC_PACKAGE_PATH"
  createDirectoryIfMissing True workDir
  build_plan <- BS.readFile buildPlanFile >>=
    maybe (fail "failed to parse build plan") return . preview J._YAML
  let adjusted_build_plan = adjustBuildPlan
        -- (Just $ setOf (folded . T.unpacked . to mkPackageName) packages)
        Nothing
        build_plan
  withCurrentDirectory workDir $ do
    withSystemTempDirectory "experiment" $ \temp_dir -> do
      let shimSettingsFile = temp_dir </> "measure-ghc-shim-settings.yaml"
          ghcs_dir = workDir </> "ghc"
          install_dir = workDir </> "install"
      traverse_ (createDirectoryIfMissing True)
        [ ghcs_dir
        , install_dir
        ]
      setEnv "MEASURE_SHIM_FILE_ghc" shimSettingsFile
      ghc_shim_file <- fromMaybe (fail "couldn't find ghc-shim") <$> findExecutable "ghc-shim"
      current_path <- fromMaybe "" <$> lookupEnv "PATH"

      if dryRun
      then putStrLn $ adjusted_build_plan ^. re J._YAML
      else do
        for_ [ (label, ghcSpec, ghcOptions)
            | ghcSpec@GhcBindist{..} <- toList ghcs
            , (options_label, ghcOptions) <- itoList ghcsOptions
            , let label = mkLabel bindistLabel options_label
            ] $ \(label, ghcBindist@GhcBindist{..}, ghcOptions) -> do
          current_time <- getCurrentTime
          jobs_sem <- newQSemN jobs
          let time_string = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%S")) current_time
              run_dir = workDir </> "runs" </> time_string
              stats_dir = run_dir "stats"
              log_dir = run_dir "logs"
              ghc_bin_dir = ghcs_dir </> T.unpack bindistLabel </> "bin"
              ghc_pkg_file = ghc_bin_dir </> "ghc-pkg"
              ghc_file = ghc_bin_dir </> "ghc"
              statsFile = stats_dir </> "stats"
              statsFileLock = stats_dir </> "lock"
              meta_stats_file = stats_dir </> "meta"
              curator_log_file = log_dir </> "curator.log"
              curator_results_file = log_dir </> "curator.results"

              shim_settings package hint shimEnabled = ShimSettings
                { executableFile = ghc_file
                , statsFile
                , statsFileLock
                , shimEnabled
                , dumpRtsStats = Nothing
                , dumpTimeStats = Nothing
                , hint
                , package
                }
              write_settings package_name build_type enabled =
                BS.writeFile shimSettingsFile $
                  J._YAML # shim_settings package_name build_type enabled
              wait_on_dep = bracket_ (signalQSemN jobs_sem 1) (waitQSemN jobs_sem 1)
              on_setup package_name build_type command inner = do
                let write_settings' = write_settings package_name (Just build_type)
                if command == "build"
                then bracket (signalQSemN jobs_sem 1) (waitQSemN jobs_sem 1) $ do
                  let enter = waitQSemN jobs_sem jobs
                      exit = do
                        write_settings' False
                        signalQSemN jobs_sem jobs
                  bracket_ enter exit $ do
                    write_settings' True
                    inner $
                      [ "--ghc-options=" <> (T.intercalate " " ghcOptions)
                      , "--with-ghc=" <> T.pack ghc_shim_file
                      , "--with-ghc-pkg=" <> T.pack ghc_pkg_file
                      ]
                else inner []
              new_build_plan = adjusted_build_plan
                & #bpPackages . traverse . #ppConstraints . #pcConfigureArgs <>~ toVector
                  [ "--allow-newer"  -- should limit this and the next to built in packages
                  , "--allow-older"
                  ]
              log_bytestring h bs = T.hPutStrLn h . fromMaybe "<not utf8> " . preview strictUtf8
          for_ [run_dir, stats_dir, log_dir] $ \d -> do
            exists <- doesDirectoryExist d
            when exists $ fail $ "Directory exists:" <> d
            createDirectoryIfMissing True d

          ghc_installed <- doesDirectoryExist ghc_bin_dir
          unless ghc_installed $ installGhc bindistDir ghcs_dir temp_dir ghcBindist

          removePathForcibly install_dir

          setEnv "PATH" $ intercalate ":" [ghc_bin_dir, current_path]

          write_settings "unconfigured-package" Nothing False

          BS.writeFile meta_stats_file $ J._YAML # MetaStats
            { time = current_time
            , ghcBindist
            , label
            , ghcOptions
            }
          -- we're doing our own job limiting, so we pass a ludicrous value to avoid deadlock
          withFile curator_log_file WriteMode $ \curator_log_handle -> do
            performAdjustedBuild new_build_plan 50 install_dir (BS.hPutLine curator_log_handle) log_dir on_setup wait_on_dep >>= T.writeFile curator_results_file . T.unlines
{- for_ runs $ \(label, ghc, ghc_options) -> do
        try_ $ callProcess "cabal" ["sandbox", "delete"]
        callProcess "cabal" ["sandbox", "init"]
        let configure_opts =
              [ "--with-ghc=ghc-shim"
              , "--with-ghc-pkg=" <> (ghc ^. _2)
              , "--ghc-options=" <> (T.unpack $ T.intercalate " " ghc_options)
              , "--allow-newer"
              ]
            shim_settings_enabled = ShimSettings
              { _ss_executableFile = ghc ^. _1
              , _ss_statsFile
              , _ss_statsFileLock
              , _ss_enabled = True
              , _ss_dumpRtsStats = Nothing
              , _ss_dumpTimeStats = Nothing
              }
            shim_settings_disabled = shim_settings_enabled & enabled .~ False
        BS.writeFile shimSettingsFile $ shim_settings_disabled ^. re J._YAML
        cabal_output_str <- readProcess "cabal" ?? "" $
          fold
            [ ["install"]
            , opts ^.. #packages . traverse . T.unpacked
            , ["--dry-run"]
            , configure_opts
            ]
        let packages = filter (not . null) . drop 2 . lines $ cabal_output_str
        for_ packages $ \package -> do
          try_ $ callProcess "cabal" ["unpack", package]
          withCurrentDirectory package $ do
            try_ $ callProcess "cabal" ["clean"]
            callProcess "cabal" ["sandbox", "init", "--sandbox", pwd </> ".cabal-sandbox"]
            callProcess "cabal" $ "configure" : configure_opts
            BS.writeFile shimSettingsFile $ shim_settings_enabled ^. re J._YAML
            callProcess "cabal" ["build"]
            BS.writeFile shimSettingsFile $ shim_settings_disabled ^. re J._YAML
            callProcess "cabal" ["copy"]
            callProcess "cabal" ["register"]
-}
