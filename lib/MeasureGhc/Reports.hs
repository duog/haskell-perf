{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MeasureGhc.Reports where

import Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.ToField as S
import qualified Data.Csv as C
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Strict as Strict
import qualified System.Console.Terminal.Size as SCTS
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import System.IO (stdout)
import qualified Data.Map.Lazy as M
import MeasureGhc.Prelude
import qualified MeasureGhc.Text as T
import qualified MeasureGhc.ByteString as B
import MeasureGhc.Reports.Types
import MeasureGhc.Options.Types
import MeasureGhc.Reports.Pretty
import MeasureGhc.Reports.AdHocCsv
import Paths_measure_ghc

generateReport :: (MonadThrow m, MonadIO m) => S.Connection -> ReportOptions -> m ()
generateReport conn report_opts = do
  (doc, csv) <-
    case report_opts ^. reportSpec of
      RS_PackageSamples ->
        packageSamplesReport
          conn
          (report_opts ^. whichProgram)
          (report_opts ^. whichLabels)
      RS_Improvement b -> do
        improvementsReport
          conn
          b
          (report_opts ^. whichProgram)
          (report_opts ^. whichLabels)
      rs -> throwMeasureGhc $ "unimplemented report" <> showText rs
  case report_opts ^. outputFormat of
    RF_Csv -> liftIO . B.putStr $ csv
    RF_Pretty mb_w0 -> do
      mb_w <-
        runMaybeT $ hoistMaybe mb_w0 <|>
        (liftIO SCTS.size & MaybeT . (mapped . mapped %~ SCTS.width))
      liftIO $ putStrLn $ "using width:" <> show mb_w
      let simple_doc =
            maybe PP.renderCompact (\w -> PP.renderPretty 1.0 w) mb_w doc
      liftIO $ PP.displayIO stdout simple_doc
  -- = do
  -- for_ [minBound..maxBound] $ \p -> do
  --   mb_program_id <- getProgramId conn p
  --   for_ mb_program_id $ \program_id ->
  --     badPackagesReport conn program_id >>= traverse (liftIO . T.putStrLn . showText)


transformNonEmpty :: AsEmpty a => (a -> a) -> a -> a
transformNonEmpty f = \case
  Empty -> Empty
  x -> f x


newtype QueryT m a = QueryT (Strict.WriterT (Seq Text, Seq S.SQLData) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runQueryT
  :: (MonadIO m, S.FromRow b, Monoid c)
  => QueryT m a -> S.Connection -> (a -> b -> IO c) -> m (BVector Text, c)
runQueryT (QueryT sm) conn f = do
  (a, (ts, ds)) <- Strict.runWriterT sm
  liftIO $
    S.withStatement
      conn
      (S.Query . view (from T.builder) . foldOf (folded . T.builder) $ ts) $ \stmt -> do
      num_cols <- S.columnCount stmt
      col_names <-
        for [0 .. num_cols - 1] (S.columnName stmt) & mapped %~
        toVectorOf folded
      S.bind stmt . toList $ ds
      let go c = S.nextRow stmt >>= maybe (return c) (f a >=> go . mappend c)
      r <- go mempty
      return (col_names, r)

queryQueryT :: (MonadIO m, S.FromRow r) => S.Connection -> QueryT m () -> m [r]
queryQueryT conn q = runQueryT q conn (\_ r -> pure [r]) & mapped %~ view _2

queryWithColumnNamesQueryT :: (MonadIO m, S.FromRow r) => S.Connection -> QueryT m () -> m (BVector Text, [r])
queryWithColumnNamesQueryT conn q = runQueryT q conn (\_ r -> pure [r])

queryFragment :: (Monad m, S.ToField d) => Text -> d -> QueryT m ()
queryFragment t d = QueryT $ do
  tell (pure t, pure . S.toField $ d)

queryFragment_ :: Monad m => Text -> QueryT m ()
queryFragment_ t = QueryT $ tell (pure t, mempty)

queryIfAny :: Monad m => ([Text] -> Text) -> QueryT m a -> QueryT m a
queryIfAny f (QueryT m) = QueryT $ do
  (a, w@(ts, vs)) <- listen m
  when (_Empty `hasn't` vs) $ tell (pure . f . toList $ ts, vs)
  pure a

queryWhereClause :: (Monad m) => [[QueryT m a]] -> QueryT m [[a]]
queryWhereClause ands_of_ors = mk_and_query . fmap mk_or_query $ ands_of_ors
  where
    mk_and_query = traverse $ queryIfAny (\ss -> " WHERE " <> T.intercalate " AND " ss)
    mk_or_query = traverse $ queryIfAny (\ss -> T.intercalate " OR " ["(" <> s <> ")" | s <- ss])

queryWhereClause_ :: Monad m => [[QueryT m a]] -> QueryT m ()
queryWhereClause_ = void . queryWhereClause


improvementsReport
  :: (MonadThrow m, MonadIO m)
  => S.Connection -> Bool -> Maybe Program -> Seq Text -> m (PP.Doc, ByteString)
improvementsReport conn input_order_sort mb_program labels0 = do
  (labels_rows :: [(Int, Text)]) <-
    queryQueryT conn $ do
      queryFragment_
        "SELECT l.id, l.name FROM label l INNER JOIN sample s ON s.label_id = l.id INNER JOIN program p on p.id = s.program_id"
      queryWhereClause_
        [ mb_program ^.. folded . to (queryFragment "p.name = (?)")
        , labels0 ^.. folded . to (queryFragment "l.name = (?)")
        ]
      queryFragment_ " GROUP BY l.id, l.name HAVING COUNT(*) > 0 ORDER BY l.id"
  let present_label_set = setOf (folded . _2) labels_rows
      labels_sort_order_map =
        M.fromList $ labels0 ^.. ifolded . withIndex . swapped
      (absent_labels, label_ids) =
        partitionEithers
          [ if present_label_set ^. contains l
            then Right i
            else Left l
          | (i, l, _) <-
              sortBy
                (compare `on`
                 view
                   (if input_order_sort
                      then _3
                      else _1))
                [ ( i
                  , l
                  , fromMaybe maxBound . preview (ix l) $ labels_sort_order_map)
                | (i, l) <- labels_rows
                ]
          ]
      num_labs = length label_ids
  when (not . null $ absent_labels) $ throwMeasureGhc $ "Missing labels:" <>
    T.intercalate ", " absent_labels
  when (num_labs < 2) $ throwMeasureGhc $ "Need at least two labels, have:" <>
    showText num_labs
  (col_names, rows :: [(Text, Text) S.:. [Double]]) <-
    queryWithColumnNamesQueryT conn $ do
      let mk_l_tn = ("l" <>) . showText
          head_id = label_ids ^?! _head
          head_tn = mk_l_tn head_id
      queryFragment_ $ "SELECT " <> head_tn <> ".program_name, l" <> head_tn <>
        ".package_name"
      for_
        [ "bytes_allocated_avg"
        , "total_cpu_seconds_avg"
        , "total_wall_seconds_avg"
        ] $ \col_name -> do
        let mk_l_cn n = mk_l_tn n <> "." <> col_name
            mk_improvement_c cn1 cn2 s =
              fold
              [", 100 * (", cn1, " - ", cn2, ") / ", cn1, " as ", cn2, "_", s]
        when (num_labs > 2) $ for_ (zip label_ids (tail label_ids)) $ \(l1, l2) -> do
          let l1_cn = mk_l_cn l1
              l2_cn = mk_l_cn l2
          queryFragment_ $ mk_improvement_c l1_cn l2_cn "improvement"
        let fst_cn = mk_l_cn $ label_ids ^?! _head
            lst_cn = mk_l_cn $ label_ids ^?! _last
        queryFragment_ $ mk_improvement_c fst_cn lst_cn "total_improvement"
      queryFragment
        (" FROM (SELECT * FROM package_stats WHERE label_id = (?)) as " <>
         head_tn)
        head_id
      for_ (zip label_ids (tail label_ids)) $ \(l1, l2) -> do
        let l1_tn = mk_l_tn l1
            l2_tn = mk_l_tn l2
        queryFragment
          (" INNER JOIN (SELECT * FROM package_stats WHERE label_id = (?)) as " <>
           l2_tn <>
           " ON " <>
           l1_tn <>
           ".program_name = " <>
           l2_tn <>
           ".program_name AND " <>
           l1_tn <>
           ".package_name = " <>
           l2_tn <>
           ".package_name")
          l2
      queryWhereClause_
        [mb_program ^.. to (queryFragment (head_tn <> ".program_name = (?)"))]
  let csv =
        withMkAdHoc (col_names & mapped %~ (view $ T.unpacked . B.packedChars)) $ \mk_rec ->
          C.encodeDefaultOrderedByName
            [ mk_rec . toVectorOf folded $ [B.Chars c1, B.Chars c2] <> c3s ^..
            folded .
            to show .
            B.packedChars
            | (T.Text c1, T.Text c2) S.:. c3s <- rows
            ]
  let improvement_checker v =
        if | v < -2.5 -> GT
           | v < 2.5 -> LT
           | otherwise -> EQ
      pretty =
        buildReport
          col_names
          [ toVectorOf
            (folded . folded)
            [ [(id, c1), (id, c2)] <> c3s ^.. folded .
              to (mkDoubleCell improvement_checker 1)
            ]
          | (c1, c2) S.:. c3s <- rows
          ]
  return (pretty, csv ^. strict)

packageSamplesReport
  :: MonadIO m
  => S.Connection
  -> Maybe Program
  -> Seq Text
  -> m (PP.Doc, ByteString)
packageSamplesReport conn mb_program labels = do
  (col_names, rows) <-
    queryWithColumnNamesQueryT conn $ do
      queryFragment_
        "SELECT program_name, label_name, package_name, sample_count"
      queryFragment_ . fold $
        ["bytes_allocated", "total_cpu_seconds", "total_wall_seconds"] >>=
        mk_stats_column_names
      queryFragment_ " FROM package_stats"
      queryWhereClause_
        [ mb_program ^.. folded . to (queryFragment "program_name = (?)")
        , labels ^.. folded . to (queryFragment "label_name = (?)")
        ]
  let csv = C.encodeDefaultOrderedByNameWith C.defaultEncodeOptions rows
  return
    (buildReport col_names . fmap packageSamplesReportRow $ rows, csv ^. strict)
  where
    mk_stats_column_names :: Text -> [Text]
    mk_stats_column_names c =
      [", " <> c <> "_" <> s | s <- ["avg", "max", "min", "stdevpc"]]

getProgramId :: MonadIO m => S.Connection -> Program -> m (Maybe Int)
getProgramId conn p = liftIO $ do
  r <- S.queryNamed conn "SELECT id FROM PROGRAM WHERE name = :program_name" [":program_name" := renderProgram p]
  return $ r ^? _head . to S.fromOnly

badPackagesReport :: MonadIO m => S.Connection -> Int -> m ([(Text, Text)])
badPackagesReport conn program_id = liftIO $ do
  q <- getDataFileName "data/BadPackagesReport.sql" >>= T.readFile >>= return . S.Query
  r <- S.queryNamed conn q [":program_id" := program_id]
  return $ r & traverse %~ \[a, b] -> (a, b)
