{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts,
    OverloadedStrings, FlexibleInstances, FunctionalDependencies #-}

module MeasureGhc.Options.Types where

import Database.SQLite.Simple.ToField

import MeasureGhc.Prelude

data Program = P_Ghc | P_Haddock
  deriving (Show, Eq, Ord, Enum, Bounded)

renderProgram :: Program -> Text
renderProgram = \case
  P_Ghc -> "ghc"
  P_Haddock -> "haddock"

instance ToField Program where
  toField = toField . renderProgram

data LoadOptions = LoadOptions
  { _lo_whichProgram :: Program
  , _lo_samplesLabel :: Text
  , _lo_inputFilename :: FilePath
  }
makeLensesWith underscoreFields ''LoadOptions

data ReportSpec = RS_PackageSamples | RS_BadPackages | RS_Improvement Bool
  deriving (Show)

data ReportFormat = RF_Csv | RF_Pretty (Maybe Int)
  deriving (Show)

data ReportOptions = ReportOptions
  { _ro_reportSpec :: ReportSpec
  , _ro_outputFormat :: ReportFormat
  , _ro_whichProgram :: Maybe Program
  , _ro_whichLabels :: Seq Text
  } deriving (Show)
makeLensesWith underscoreFields ''ReportOptions

data SqlOptions = SqlOptions
  { _so_allargs :: [Text]
  }
makeLensesWith underscoreFields ''SqlOptions

data Mode = LoadMode LoadOptions | ReportMode ReportOptions | SqlMode SqlOptions

data Options = Options
  { _o_dbFilename :: FilePath
  , _o_modeOptions :: Mode
  }
makeLensesWith underscoreFields ''Options
