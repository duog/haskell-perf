{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts,
    GADTs, OverloadedStrings, FlexibleInstances, FunctionalDependencies #-}

module MeasureGhc.Reports.Types where

import qualified GHC.Generics as G
import qualified Data.Csv as C
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S

import MeasureGhc.Prelude
import qualified MeasureGhc.Text as T
import qualified MeasureGhc.ByteString as B
import MeasureGhc.Options.Types

data RowStyle = RS_Header | RS_Row | RS_AlternateRow

-- data PackageSamplesReportOptions = PackageSamplesReportOptions
--   { _psro_program :: Maybe Program
--   , _psro_label :: Maybe Text
--   , _psro_reportFormat :: ReportFormat
--   }
-- makeLensesWith underscoreFields ''PackageSamplesReportOptions

data SamplesStats a = SamplesStats
 { _ss_sampleAverage :: Double
 , _ss_sampleMax :: a
 , _ss_sampleMin :: a
 , _ss_sampleStdDevPercent :: Double
 } deriving (G.Generic)
makeLensesWith underscoreFields ''SamplesStats

instance C.DefaultOrdered (SamplesStats a) where
  headerOrder _ = toVectorOf (folded . B.packedChars) ["average", "stddev%"]

instance C.ToNamedRecord (SamplesStats a) where
  toNamedRecord ss =
    let h = C.headerOrder ss
    in Empty
        & at (h ^?! ix 0) ?~ (ss ^. sampleAverage . re _Show . B.packedChars)
        & at (h ^?! ix 1) ?~ (ss ^. sampleStdDevPercent . re _Show . B.packedChars)

instance S.FromField a => S.FromRow (SamplesStats a) where
  fromRow = SamplesStats <$> S.field <*> S.field <*> S.field <*> S.field

data PackageSamplesReportRow = PackageSamplesReportRow
  { _psrr_programName :: Text
  , _psrr_labelName :: Text
  , _psrr_packageName :: Text
  , _psrr_sampleCount :: Int
  , _psrr_bytesAllocated :: SamplesStats Int
  , _psrr_totalCpuTime :: SamplesStats Double
  , _psrr_totalWallTime :: SamplesStats Double
  } deriving (G.Generic)
makeLensesWith underscoreFields ''PackageSamplesReportRow

instance C.DefaultOrdered PackageSamplesReportRow where
  headerOrder _ = toVectorOf (folded . B.packedChars) $
    [ "program"
    , "label"
    , "package"
    , "samples count"
    ] <> foldMap (\(v, p) -> v ^.. folded . B.unpackedChars . to (p <>))
    [ ((C.headerOrder (undefined :: SamplesStats Int)), "bytes allocated ")
    , ((C.headerOrder (undefined :: SamplesStats Double)), "cpu time ")
    , ((C.headerOrder (undefined :: SamplesStats Double)), "wall time ")
    ]

instance C.ToNamedRecord PackageSamplesReportRow where
  toNamedRecord x =
    let h = C.headerOrder x
        mk_stats_record p = _Wrapped . traverse . _1 %~ (p <>)
    in review _Wrapped . zip (toList h) $
         [ x ^. programName . T.unpacked . B.packedChars
         , x ^. labelName . T.unpacked . B.packedChars
         , x ^. packageName . T.unpacked . B.packedChars
         , x ^. sampleCount . re _Show . B.packedChars
         ] <> fold
         [ x ^.. bytesAllocated . to C.toNamedRecord . folded
         , x ^.. totalCpuTime . to C.toNamedRecord . folded
         , x ^.. totalWallTime . to C.toNamedRecord . folded
         ]

instance S.FromRow PackageSamplesReportRow where
  fromRow = PackageSamplesReportRow
    <$> S.field
    <*> S.field
    <*> S.field
    <*> S.field
    <*> S.fromRow
    <*> S.fromRow
    <*> S.fromRow
