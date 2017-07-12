{-# LANGUAGE OverloadedStrings #-}
module MeasureGhc.Options
  ( module MeasureGhc.Options.Types
  , module MeasureGhc.Options
  ) where

import qualified Options.Applicative as O

import MeasureGhc.Prelude
import MeasureGhc.Options.Types

programParser :: O.Parser Program
programParser = (O.flag' P_Ghc $ O.short 'g' <> O.long "ghc") <|> (O.flag' P_Haddock $ O.short 'a' <> O.long "haddock")

labelParser :: O.Parser Text
labelParser = O.strOption $ O.short 'l' <> O.long "label"

loadOptionsParser :: O.Parser LoadOptions
loadOptionsParser = LoadOptions
  <$> programParser
  <*> labelParser
  <*> O.strOption (O.short 'i' <> O.long "input-file")

reportSpecParser :: O.Parser ReportSpec
reportSpecParser =
  asum
    [ package_samples_parser
    , bad_packages_parser
    , improvement_parser
    , pure . RS_Improvement $ False
    ]
  where
    package_samples_parser =
      O.flag' RS_PackageSamples $ O.long "package-samples"
    bad_packages_parser = O.flag' RS_BadPackages $ O.long "bad-packages"
    improvement_parser = (naked_improvement_parser <|> pure RS_Improvement) <*> input_order_labels_parser
    input_order_labels_parser = O.switch $ O.long "order-labels-input"
    naked_improvement_parser =  O.flag' RS_Improvement $ O.long "improvement"

reportFormatParser :: O.Parser ReportFormat
reportFormatParser =
  csv_parser <|> ((pretty_parser <|> pure RF_Pretty) <*> optional pretty_width_parser)
  where
    csv_parser = O.flag' RF_Csv $ O.short 'c' <> O.long "csv"
    pretty_parser = O.flag' (RF_Pretty) $ O.short 'p' <> O.long "pretty"
    pretty_width_parser =
      O.option O.auto (O.short 'w' <> O.long "pretty-width")

reportOptionsParser :: O.Parser ReportOptions
reportOptionsParser = ReportOptions
  <$> reportSpecParser
  <*> reportFormatParser
  <*> optional programParser
  <*> (seqOf folded <$> many labelParser)
  where

sqlOptionsParser :: O.Parser SqlOptions
sqlOptionsParser = SqlOptions <$> many (O.strArgument $ O.metavar "SQL")

optionsParser :: O.Parser Options
optionsParser = Options
  <$> O.strOption (O.short 'd' <> O.long "database" <> O.metavar "DATABASE_FILE" <> O.help "The sqllite database file")
  <*> modeOptionsParser
  where
    modeOptionsParser = O.subparser $ fold
      [ O.command "load" $ O.info (fmap LoadMode $ O.helper <*> loadOptionsParser) $ O.progDesc "Load a data file into db"
      , O.command "report" $ O.info (fmap ReportMode $ O.helper <*> reportOptionsParser) $ O.progDesc "Generate a report"
      , O.command "sql" $ O.info (fmap SqlMode $ O.helper <*> sqlOptionsParser) $ O.progDesc "Query DB" <> O.forwardOptions
      ]

optionsParserInfo :: O.ParserInfo Options
optionsParserInfo = O.info (O.helper <*> optionsParser) $
  O.progDesc "measure ghc performance on a number of packages across ghc versions"

execOptionsParser :: MonadIO m => m Options
execOptionsParser = liftIO $ O.execParser optionsParserInfo
