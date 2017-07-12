{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module MeasureGhc.Main (main) where

import System.IO
import System.Directory
import System.FilePath (FilePath)
import System.FilePath.Lens
import qualified Data.Map.Lazy as M
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.Internal as S
import qualified Database.SQLite3 as SD
import qualified Database.SQLite3.Direct as S
import Sqlite.Ext.Register
import Database.SQLite.Simple (NamedParam((:=)))
import MeasureGhc.Prelude
import qualified MeasureGhc.Text as T
import MeasureGhc.Options
import MeasureGhc.Reports

import Paths_measure_ghc
data PackageReport = PackageReport
  { prDir :: FilePath
  , prCommand ::Text
  , prStats :: Map Text Text
  }

isNotSubstringOf :: Text -> Text -> Bool
isNotSubstringOf needle haystack =
  let (_, r) = T.breakOn needle haystack
  in T.null r

parsePackageReports :: (Text -> Bool) -> [Text] -> [(Text, Either Text PackageReport)]
parsePackageReports command_line_filter xs =
  [ pr
  | ys <- chunk_input $ xs ^.. folded . filtered (not . T.null)
  , Just pr <- [package_report ys]
  ]
  where
    chunk_input ys =
      let (first, rest) =
            span (hasn't $ _head . only '=') $
            dropWhile (has $ _head . only '=') ys
      in if null first
         then []
         else first : chunk_input rest
    package_report ys
      | T.Text prDir : prCommand : stats <- ys
      , command_line_filter prCommand
      = Just
        ( prDir ^. basename . T.packed
        , case
            preview (T.unpacked . _Show ) (fold stats)
              & traverse . traverse %~ (each %~ T.pack)
            of
            Just (review _Wrapped -> prStats) -> pure PackageReport{..}
            Nothing -> Left $ "Failed to parse:" <> fold stats
        )
      | T.Text prDir : [] <- ys = Just (prDir ^. basename . T.packed , Left $ "not enough lines:" <> T.Text prDir)
      | otherwise = Nothing

filterCommandLine :: Program -> Text -> Bool
filterCommandLine P_Ghc cmdLine = all (isNotSubstringOf ?? cmdLine) ["--numeric-version", "--supported-languages", "--info", "--print-libdir", " -c ", " -o ", "-dynload deploy", "/setup", "--abi-hash", "-no-link"]
filterCommandLine P_Haddock cmdLine = all (isNotSubstringOf ?? cmdLine) ["--version", "--ghc-version", "--gen-contents"]


loadInputFile :: MonadIO m => Program -> FilePath -> m (Map Text PackageReport)
loadInputFile program input_file = do
  fileLines <- liftIO $ T.readFile input_file <&> T.lines
  let packageReports = parsePackageReports (filterCommandLine program) fileLines
      (error_packages, good_packages) = partitionEithers $
        packageReports <&> \(p, x) -> bimap (p,) (p,) x
  liftIO $ for_ error_packages $ uncurry logError
  let map_with_multiple_packages = M.fromListWith (<>) $ good_packages & mapped . _2 %~ pure
  fmap (M.mapMaybe id) . flip M.traverseWithKey map_with_multiple_packages $ \p -> \case
    [pr] -> pure $ Just pr
    prs -> do
      logError p $ "Not unique. #:" <> (length prs ^. re _Show . T.packed)
      pure Nothing
  where
    logError p e = liftIO $ T.hPutStrLn stderr $ fold ["error:", T.Text input_file, ":", p, ":", e]

-- packageReportColumns :: PackageReport -> (Text, Text)
-- packageReportColumns PackageReport{..} =
--   ( showText $ pickColumn "bytes allocated"
--   , showText $ pickColumn "init_cpu_seconds" + pickColumn "mutator_cpu_seconds" + pickColumn "GC_cpu_seconds"
--   )
--   where pickColumn x = prStats ^?! ix x

ensureProgramAndLabelAndSampleFile :: MonadIO m => S.Connection -> LoadOptions -> m (Int, Int, Int)
ensureProgramAndLabelAndSampleFile conn load_opts = liftIO $ do
  let program = load_opts ^. whichProgram . to renderProgram
      label = load_opts ^. samplesLabel
      sample_file = load_opts ^. inputFilename
  S.executeNamed conn "INSERT OR IGNORE INTO program (name) VALUES (:program)" [":program" := program]
  [[program_id]] <- S.queryNamed conn "SELECT id FROM program where name = :program" [":program" := program]
  S.executeNamed conn "INSERT OR IGNORE INTO label (name) VALUES (:label)" [":label" := label]
  [[label_id]] <- S.queryNamed conn "SELECT id FROM label where name = :label" [":label" := label]
  S.executeNamed conn "INSERT OR IGNORE INTO sample_file (name) VALUES (:sample_file)" [":sample_file" := sample_file]
  [[sample_file_id]] <- S.queryNamed conn "SELECT id FROM sample_file where name = :sample_file" [":sample_file" := sample_file]
  S.executeNamed conn "DELETE FROM sample where sample_file_id = :sample_file_id" [":sample_file_id" := sample_file_id]
  return (program_id, label_id, sample_file_id)

uploadReport :: MonadIO m => S.Connection -> (Int, Int, Int) ->Text -> PackageReport -> m ()
uploadReport conn (program_id, label_id, sample_file_id) package PackageReport{..} = liftIO $ do
  S.executeNamed conn "INSERT OR IGNORE INTO package (name) VALUES (:package)" [":package" := package]
  [[package_id :: Int]] <- S.queryNamed conn "SELECT id FROM package where name = :package" [":package" := package]
  S.executeNamed conn (fold
    [ "INSERT INTO sample ( program_id, label_id, package_id, sample_file_id, pwd, command_line, bytes_allocated, num_GCs, average_bytes_used, max_bytes_used, num_byte_usage_samples, peak_megabytes_allocated, init_cpu_seconds, init_wall_seconds, mutator_cpu_seconds, mutator_wall_seconds, GC_cpu_seconds, GC_wall_seconds)"
    , "VALUES             (:program_id,:label_id,:package_id,:sample_file_id,:pwd,:command_line,:bytes_allocated,:num_GCs,:average_bytes_used,:max_bytes_used,:num_byte_usage_samples,:peak_megabytes_allocated,:init_cpu_seconds,:init_wall_seconds,:mutator_cpu_seconds,:mutator_wall_seconds,:GC_cpu_seconds,:GC_wall_seconds)"
    ]) $ [":program_id" := program_id
      , ":label_id" := label_id
      , ":package_id" := package_id
      , ":sample_file_id" := sample_file_id
      , ":command_line" := prCommand
      , ":pwd" := prDir
      ] <> [(":" <> T.replace " " "_" k) := v | (k, v) <- itoList prStats]

initDb :: MonadIO m => S.Connection -> m ()
initDb _conn@(S.Connection handle) = liftIO $ do
  void $ registerExtensionFunctions handle
  for_
    [ "data/ensure_tables.sql"
    , "data/create_view_package_stats.sql"
    ] $ getDataFileName >=> T.readFile >=> SD.exec handle

runOptions :: MonadIO m => Options -> m ()
runOptions opts = do
  liftIO $ S.withConnection (opts ^. dbFilename) $ \conn -> do
    initDb conn
    case opts ^. modeOptions of
      LoadMode load_opts -> do
        reports <- loadInputFile (load_opts ^. whichProgram) (load_opts ^. inputFilename)
        sundry_ids <- ensureProgramAndLabelAndSampleFile conn load_opts
        ifor_ reports $ uploadReport conn sundry_ids
      ReportMode reportOpts -> do
        generateReport conn reportOpts
      SqlMode sqlOpts -> do
        q <- case sqlOpts ^. allargs of
          [] -> getContents <&> T.Text
          os -> return $ T.intercalate " " os
        SD.execPrint (S.connectionHandle conn) q

main :: IO ()
main = do
  execOptionsParser >>= runOptions
