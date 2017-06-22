{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import System.IO
import System.Directory
import Control.Lens
import System.FilePath (FilePath)
import System.FilePath.Lens
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lens as T
import qualified Data.Text.IO as T
import Data.Sequence (Seq)
import Data.List
import Data.List.Lens
import Data.Map (Map)
import Control.Monad.IO.Class
import qualified Data.Map.Lazy as M
import Data.Foldable
import Data.Set.Lens
import Data.Either
import Data.Semigroup
import Data.Bitraversable
import Data.Traversable

data InputFile = InputFile
  { inputFilename :: FilePath
  , inputLabel :: Text
  }

_InputFile :: Prism' FilePath InputFile
_InputFile = prism' inputFilename $ \f ->
  case f ^? basename . prefixed "haddock-perf-" of
    Just l -> return $ InputFile f $ T.Text l
    Nothing -> Nothing

data PackageReport = PackageReport
  { prDir :: FilePath
  , prCommand ::Text
  , prStats :: Map Text Double
  }

isNotSubstringOf :: Text -> Text -> Bool
isNotSubstringOf needle haystack =
  let (_, r) = T.breakOn needle haystack
  in T.null r

parsePackageReports :: [Text] -> [(Text, Either Text PackageReport)]
parsePackageReports xs =
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
      , all id [x `isNotSubstringOf` prCommand | x <- ["--version", "--ghc-version", "--gen-contents"]]
      = Just
        ( prDir ^. basename . T.packed
        , case
            preview (T.unpacked . _Show) (fold stats) >>=
              traverse (bitraverse (pure . T.pack) (preview _Show))
            of
            Just (review _Wrapped -> prStats) -> pure PackageReport{..}
            Nothing -> Left $ "Failed to parse:" <> fold stats
        )
      | T.Text prDir : [] <- ys = Just (prDir ^. basename . T.packed , Left $ "not enough lines:" <> T.Text prDir)
      | otherwise = Nothing

loadInputFile :: MonadIO m => InputFile -> m (Map Text PackageReport)
loadInputFile InputFile{..} = do
  fileLines <- liftIO $ T.readFile inputFilename <&> T.lines
  let packageReports = parsePackageReports fileLines
      (error_packages, good_packages) = partitionEithers $
        packageReports <&> \(p, x) -> bimap (p,) (p,) x
  liftIO $ for_ error_packages $ uncurry logError
  let map_with_multiple_packages = M.fromListWith (<>) $ good_packages & mapped . _2 %~ pure
  M.traverseMaybeWithKey ?? map_with_multiple_packages $ \p -> \case
    [pr] -> pure $ Just pr
    prs -> do
      logError p $ "Not unique. #:" <> (length prs ^. re _Show . T.packed)
      pure Nothing
  where
    logError p e = liftIO $ T.hPutStrLn stderr $ fold ["error:", inputLabel, ":", p, ":", e]
packageReportColumns :: PackageReport -> (Text, Text)
packageReportColumns PackageReport{..} =
  ( showText $ pickColumn "bytes allocated"
  , showText $ pickColumn "init_cpu_seconds" + pickColumn "mutator_cpu_seconds" + pickColumn "GC_cpu_seconds"
  )
  where pickColumn x = prStats ^?! ix x

showText :: Show a => a -> Text
showText = T.Text . show

main :: IO ()
main = do
  this_dir_files <- getCurrentDirectory >>= listDirectory
  let input_files = this_dir_files ^.. folded . _InputFile
  report_maps <- traverse loadInputFile input_files
  let all_packages_set = setOf (folded . ifolded . asIndex) report_maps
  T.putStrLn $ T.intercalate "," $ ["package"] <> concat [[l <> "-allocations", l <> "-time"] | InputFile _ l <- input_files]
  for_ all_packages_set $ \p -> do
    T.putStrLn $ T.intercalate "," $ (p :) $ report_maps >>= toListOf each . maybe ("0", "0") packageReportColumns . preview (ix p)
