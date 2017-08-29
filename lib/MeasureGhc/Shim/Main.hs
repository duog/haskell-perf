{-# LANGUAGE OverloadedStrings, OverloadedLabels, TemplateHaskell, DeriveAnyClass, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module MeasureGhc.Shim.Main where

import System.Environment
import System.FilePath
import System.Process
import System.FileLock
import System.IO.Temp
import GHC.Generics hiding (to, from)
import System.IO
import System.Directory
import Data.Time.Clock

import MeasureGhc.Schema.Types
import Data.Generics.Labels
import MeasureGhc.Prelude

import qualified MeasureGhc.JSON as A
import qualified MeasureGhc.Text as T
import qualified MeasureGhc.ByteString as BS

parseRtsStats :: MonadThrow m => BS.ByteString -> m RtsStats
parseRtsStats bs = do
  let mb_stats_map :: Maybe (Map String String)
      mb_stats_map = bs ^? BS.unpackedChars . (to $ fold . drop 1 . lines) . _Show . _Unwrapped
      problem = throwM $ userError "failed to parse rts stats"
  fromMaybeT problem $ do
    stats_map <- hoistMaybe mb_stats_map
    let lookup n = do
          v <- hoistMaybe . preview (ix n) $ stats_map
          hoistMaybe $ readMaybe v
    bytesAllocated <- lookup "bytes allocated"
    numGCs <- lookup "bytes allocated"
    averageBytesUsed <- lookup "average_bytes_used"
    maxBytesUsed <- lookup "max_bytes_used"
    numByteUsageSamples <- lookup "num_byte_usage_samples"
    peakMegabytesAllocated <- lookup "peak_megabytes_allocated"
    initCpuSeconds <- lookup "init_cpu_seconds"
    initWallSeconds <- lookup "init_wall_seconds"
    mutatorCpuSeconds <- lookup "mutator_cpu_seconds"
    mutatorWallSeconds <- lookup "mutator_wall_seconds"
    gcCpuSeconds <- lookup "GC_cpu_seconds"
    gcWallSeconds <- lookup "GC_wall_seconds"
    return RtsStats{..}

timeFormatString :: String
timeFormatString = unlines
  [ "averageUnsharedDataKb: %D"
  , "majorPageFaultCount: %F"
  , "fileInputsCount: %I"
  , "averageTotalMemKb: %K"
  , "maxResidentSizeKb: %M"
  , "fileOutputsCount: %O"
  , "cpuPercentage: %P"
  , "minorPageFaultCount: %R"
  , "systemTimeSeconds: %S"
  , "userTimeSeconds: %U"
  , "swappedOutCount: %W"
  , "averageSharedTextKb: %X"
  , "systemPageSizeBytes: %Z"
  , "involuntaryContextSwitchCount: %c"
  , "wallTimeSeconds: %e"
  , "signalsReceivedCount: %k"
  , "averageUnsharedStackKb: %p"
  , "socketMessagesReceivedCount: %r"
  , "socketMessagesSentCount: %s"
  , "averageResidentSizeKb: %t"
  , "voluntaryContextSwitchCount: %w"
  ]


loadSettings :: MonadIO m => String -> m (ShimSettings)
loadSettings program_name = liftIO $ do
  mb_settings_file <- lookupEnv $ "MEASURE_SHIM_FILE_" <> program_name
  settings_file <-
    maybe
      (fail "no MEASURE_GHC_SHIM_FILE environment variable")
      return
      mb_settings_file
  BS.readFile settings_file
    >>= maybe (fail $ "failed to parse '") return . preview A._YAML

defaultMain :: String -> IO ()
defaultMain program_name = do
  args <- getArgs
  ShimSettings{..}<- loadSettings program_name
  if shimEnabled
  then do
    withSystemTempDirectory ("measure-shim-" <> program_name) $ \temp_dir -> do
      let rts_stats_file = temp_dir </> "rts-stats"
          time_stats_file = temp_dir </> "time-stats"
      current_time <- getCurrentTime
      callProcess "time" $
        fold
          [ ["-f", timeFormatString, "-o", time_stats_file]
          , [executableFile]
          , ["+RTS", "-t" <> rts_stats_file, "--machine-readable", "-RTS"]
          , args
          ]
      let dump_rts_stats = fromMaybe False dumpRtsStats
          dump_time_stats = fromMaybe False dumpTimeStats
          dump = putStrLn
      when dump_rts_stats $ dump $ "reading rts stats file:" <> rts_stats_file
      rtsStats <- withFile rts_stats_file ReadMode $ \rts_stats_handle -> do
        rts_stats_bs <- BS.hGetContents rts_stats_handle
        when dump_rts_stats $ do
          dump "rts stats file:"
          dump $ rts_stats_bs ^. BS.unpackedChars
        parseRtsStats rts_stats_bs

      when dump_time_stats $ dump $ "reading time stats file:" <> time_stats_file
      timeStats <- withFile time_stats_file ReadMode $ \time_stats_handle -> do
        time_stats_bs <- BS.hGetContents time_stats_handle
        when dump_time_stats $ do
          dump "time stats file:"
          dump $ time_stats_bs ^. BS.unpackedChars
        time_stats_json0 <- maybe (fail $ "failed to parse time stats") return $
          time_stats_bs ^? A._YAML . simple @ A.Value
        time_stats_json <- maybe (fail $ "failed to tweak cpu %") return $
          time_stats_json0
            & A.key "cpuPercentage" %%~
              preview (A._String . to (T.filter (/= '%')) . A._Value)
        maybe (fail "failed to convert time stats to schema") return $ time_stats_json ^? A._YAML
      currentDir <- getCurrentDirectory
      let commandLine = (executableFile, T.Text <$> args)
          program_run = ProgramRun{time = current_time, ..}
      withFileLock statsFileLock Exclusive $ \_ ->
        withFile statsFile AppendMode $ \file_handle -> do
          BS.hPut file_handle (A._YAML # program_run)
          BS.hPut file_handle (BS.Chars "---\n")
  else callProcess executableFile args
