module MeasureGhc.Reports.Pretty where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import MeasureGhc.Prelude
import qualified MeasureGhc.Text as T
import MeasureGhc.Reports.Types

fillWith :: Char -> Int -> PP.Doc -> PP.Doc
fillWith c f d  = PP.width d $ \w ->
  if (w >= f)
  then mempty
  else PP.text $ stimes (f - w) $ [c]

-- newtype ReportColumn = ReportColumn (Store Text (Int -> Doc))

wrappingColumn :: Int -> Text -> ReportColumn
wrappingColumn width t =
  let rows = seqOf folded . T.chunksOf width $ t
  in ReportColumn (width, rows & traverse %~ ( PP.fill width . PP.text . T.unpack))

maybeWrappingColumn :: Maybe Int -> Text -> ReportColumn
maybeWrappingColumn =
  maybe (ReportColumn . (0,) . pure . PP.text . T.unpack) wrappingColumn

prettyHeaders :: BVector Text -> PP.Doc
prettyHeaders =
  prettyRow RS_Header . fmap (id,)

newtype ReportColumn = ReportColumn
  { unReportColumn :: (Int, Seq PP.Doc)
  }

_innerDoc :: Traversal' ReportColumn PP.Doc
_innerDoc = iso unReportColumn ReportColumn . _2 . traverse

runReportColumns
  :: (Foldable f)
  => PP.Doc -> f ReportColumn -> PP.Doc
runReportColumns col_sep cols =
  let pad_docs width height docs =
        let pure_empty_line = pure . PP.fill width $ mempty
        in docs <> fold (replicate (0 `max` (max_height - height)) pure_empty_line)
      (option 0 getMax -> max_height, cols_of_rows) =
        sequence
          [ (pure . pure $ height, toList . pad_docs width height $ docs)
          | ReportColumn (width, docs) <- toList cols
          , let height = length docs
          ]
      rows_of_cols = transpose cols_of_rows
  in PP.vcat
       [ fold
         [ col_sep
         , fold . intersperse col_sep. toList $ row
         , col_sep
         ]
       | row <- toList rows_of_cols
       ]

prettyRow :: RowStyle -> ReportRow -> PP.Doc
prettyRow =
  let mk_col_widths =
        memo2 $ \mb_w col_count ->
          let col_boundary =
                [ mb_w <&> \w -> ((w - 1 - col_count) * i) `div` col_count
                | i <- [0 .. col_count]
                ]
          in mzipWith (liftA2 (-)) (tail col_boundary) col_boundary
  in \rs ->
       let (style_cell, col_sep1) =
             case rs of
               RS_Header -> (PP.bold, PP.char '|')
               RS_AlternateRow ->
                 ( PP.dullwhite . PP.ondullblack
                 , PP.dullwhite . PP.ondullblack . PP.char $ '|')
               RS_Row -> (PP.white, PP.white . PP.char $ '|')
       in \cells ->
            let col_count = length cells
            in PP.columns $ \mb_width ->
                 let col_widths = mk_col_widths mb_width col_count
                 in runReportColumns col_sep1 $
                    [ maybeWrappingColumn mb_w t & _innerDoc %~
                      (style_cell . inner_style_cell)
                    | mb_w <- col_widths
                    | (inner_style_cell, t) <- toList cells
                    ]

type ReportCell = (PP.Doc -> PP.Doc, Text)

type ReportRow = BVector ReportCell

mkDoubleCell :: (Double -> Ordering) -> Int -> Double ->  ReportCell
mkDoubleCell check s =
  \d ->
    let colorize =
          case check d of
            GT -> PP.green
            EQ -> id
            LT -> PP.red
    in (colorize, T.Text $ showFFloat (Just s) d [])

sampleStatsRow :: Int -> SamplesStats a -> ReportRow
sampleStatsRow d ss =
  toVectorOf
    folded
    [ mkDoubleCell checker sigs x
    | (checker, sigs, x) <-
        [ (const EQ, d, ss ^. sampleAverage)
        , let x = ss ^. sampleStdDevPercent
              checker x
                | x > 0.05 = LT
                | otherwise = EQ
          in (checker, 2, x)
        ]
    ]

packageSamplesReportRow ::  PackageSamplesReportRow -> ReportRow
packageSamplesReportRow  psrr =
  fold
    [ toVectorOf (folded . to (id,))
      [ psrr ^. programName
      , psrr ^. labelName
      , psrr ^. packageName
      , psrr ^. sampleCount . re _Show . T.packed
      ]
    , sampleStatsRows
    ]
  where
    sampleStatsRows =
      fold
        [ sampleStatsRow 0 $ psrr ^. bytesAllocated
        , sampleStatsRow 2 $ psrr ^. totalCpuTime
        , sampleStatsRow 2 $ psrr ^. totalWallTime
        ]

buildReport :: BVector Text -> [ReportRow] ->   PP.Doc
buildReport headers rows =
  PP.vcat
    [ prettyHeaders headers
    , PP.columns $ (\i -> fillWith '=' i mempty) . fromMaybe 1
    , PP.vcat
        [ prettyRow is_alternate r
        | r <- rows
        | is_alternate <- cycle [RS_AlternateRow, RS_Row]
        ]
    ]
