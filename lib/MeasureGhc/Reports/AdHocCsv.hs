{-# LANGUAGE RankNTypes, ScopedTypeVariables, UndecidableInstances,
  FlexibleContexts, MultiParamTypeClasses, FlexibleInstances,
  TypeFamilies, TemplateHaskell #-}
module MeasureGhc.Reports.AdHocCsv where

import qualified Data.Csv as C
import Data.Reflection

import MeasureGhc.Prelude

newtype AdHocCsv s = AdHocCsv C.Record
makeWrapped ''AdHocCsv

instance Reifies s C.Header => C.DefaultOrdered (AdHocCsv s) where
  headerOrder = const $ reflect (Proxy @s)

instance Reifies s C.Header => C.ToNamedRecord (AdHocCsv s) where
  toNamedRecord r =  review _Wrapped . toList $ mzip (C.headerOrder r) (r ^. _Wrapped)

-- withHeaders :: C.Header -> (forall s. Reifies s C.Header => x) -> x
-- withHeaders h x = reify h $ const x

withMkAdHoc :: C.Header -> (forall s. Reifies s C.Header => (C.Record -> AdHocCsv s) -> a) -> a
withMkAdHoc header f = reify header $ \(_ :: Proxy s) -> f $ AdHocCsv @s
