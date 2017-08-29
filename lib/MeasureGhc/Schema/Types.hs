{-# LANGUAGE DuplicateRecordFields, TemplateHaskell, DeriveAnyClass, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module MeasureGhc.Schema.Types
  ( module MeasureGhc.Schema.Types
  , module E
  ) where

import System.FilePath
import Data.Time.Clock

import MeasureGhc.Prelude
import qualified MeasureGhc.JSON as J
import Stackage.PerformBuild as E (PBBuildType(..))

data TimeStats = TimeStats
  { averageUnsharedDataKb :: Int
  , majorPageFaultCount :: Int
  , fileInputsCount ::Int
  , averageTotalMemKb :: Int
  , maxResidentSizeKb :: Int
  , fileOutputsCount :: Int
  , cpuPercentage :: Double
  , minorPageFaultCount :: Int
  , systemTimeSeconds :: Double
  , userTimeSeconds :: Double
  , swappedOutCount :: Int
  , averageSharedTextKb :: Int
  , systemPageSizeBytes :: Int
  , involuntaryContextSwitchCount :: Int
  , wallTimeSeconds :: Double
  , signalsReceivedCount :: Int
  , averageUnsharedStackKb :: Int
  , socketMessagesReceivedCount :: Int
  , socketMessagesSentCount :: Int
  , averageResidentSizeKb :: Int
  , voluntaryContextSwitchCount :: Int
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

data RtsStats = RtsStats
  { bytesAllocated :: Int
  , numGCs :: Int
  , averageBytesUsed :: Int
  , maxBytesUsed :: Int
  , numByteUsageSamples :: Int
  , peakMegabytesAllocated :: Int
  , initCpuSeconds :: Double
  , initWallSeconds :: Double
  , mutatorCpuSeconds ::  Double
  , mutatorWallSeconds :: Double
  , gcCpuSeconds :: Double
  , gcWallSeconds :: Double
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

data ProgramRun = ProgramRun
  { currentDir :: FilePath
  , commandLine :: (FilePath, [Text])
  , rtsStats :: RtsStats
  , timeStats :: TimeStats
  , package :: Text
  , hint :: Maybe PBBuildType
  , time :: UTCTime
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

data ShimSettings = ShimSettings
  { executableFile :: FilePath
  , statsFile :: FilePath
  , statsFileLock :: FilePath
  , shimEnabled :: Bool
  , dumpRtsStats :: Maybe (Bool)
  , dumpTimeStats :: Maybe (Bool)
  , package :: Text
  , hint :: Maybe PBBuildType
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

data MetaStats = MetaStats
  { label :: Text
  , time :: UTCTime
  , ghcBindist :: GhcSpec
  , ghcOptions :: [Text]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

data GhcSpec = GhcBindist
  { bindistLabel :: Text
  , bindistDescription :: Text
  , bindistLocation :: FilePath
  } deriving (Eq, Show, Generic, J.FromJSON, J.ToJSON)
