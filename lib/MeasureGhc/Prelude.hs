{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleContexts, KindSignatures, TypeFamilies, MagicHash, TypeApplications, ScopedTypeVariables #-}
module MeasureGhc.Prelude
  ( module MeasureGhc.Prelude
  , module E
  ) where

import Control.Applicative as E
import Control.Exception.Safe as E
import Control.Lens as E hiding ((<.>))
import Control.Monad as E
import Control.Monad.IO.Class as E
import Control.Monad.State.Class as E
import Control.Monad.Writer.Class as E
import Control.Monad.Except as E
import Control.Monad.Reader as E
import Control.Monad.Trans.Maybe as E
import Control.Monad.Zip as E
-- import Control.Error.Util as E hiding ((??), tryIO)
import Data.Either as E (partitionEithers)
import Data.ByteString as E (ByteString)
import Data.Bitraversable as E
import Data.Foldable as E
import Data.Function as E
import Data.List as E hiding (uncons)
import Data.List.Lens as E
import Data.Text as E (Text)
import Data.Map as E (Map)
import Data.Maybe as E
import Data.MemoTrie as E
import Data.Proxy as E
import Data.Semigroup as E
import Data.Sequence as E (Seq)
import Data.Sequence.Lens as E hiding (sliced)
import Data.Set as E (Set)
import Data.Set.Lens as E
import Data.Traversable as E
import Data.Vector.Generic as E (Vector)
import Data.Vector.Generic.Lens as E
import GHC.Stack as E
import GHC.Generics as E (Generic)
import Numeric as E
import Text.Read as E (readMaybe)
import GHC.Prim as E (proxy#)

import qualified Data.Text as T
import qualified Data.Sequence.Lens as Seq
import qualified Data.Vector.Generic.Lens as Vector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Text.Strict.Lens as DTSL
import qualified Data.Text.Lazy.Lens as DTLL

import qualified Data.Text as Strict
import qualified Data.Text.Encoding as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import Data.Text.Encoding.Error
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

showText :: Show a => a -> T.Text
showText = T.pack . show

data MeasureGhcException = MeasureGhcException T.Text
  deriving (Typeable, Show)

instance Exception MeasureGhcException where
  displayException (MeasureGhcException t) = "measure-ghc:error:" <> T.unpack t

throwMeasureGhc :: (MonadThrow m) => T.Text -> m a
throwMeasureGhc t = throwM $ MeasureGhcException t

seqSliced :: Int -> Int -> IndexedTraversal' Int (Seq a) a
seqSliced = Seq.sliced

vectorSliced :: Vector v a => Int -> Int -> Lens' (v a) (v a)
vectorSliced = Vector.sliced

toVector :: (Vector v a, Foldable f) => f a -> v a
toVector = toVectorOf folded

type UVector = U.Vector
type BVector = V.Vector

class Strict lazy strict => AsStrict t lazy strict | t -> lazy strict where
  asStrict :: t -> strict
  default asStrict :: (strict ~ t) => t -> strict
  asStrict = id
  asLazy :: t -> lazy
  default asLazy :: (lazy ~ t) => t -> lazy
  asLazy = id
  fromStrict :: strict -> t
  default fromStrict :: (strict ~ t) => strict -> t
  fromStrict = id
  fromLazy :: lazy -> t
  default fromLazy :: (lazy ~ t) => lazy -> t
  fromLazy = id

instance AsStrict Strict.Text Lazy.Text Strict.Text where
  asLazy = Strict
  fromLazy = Lazy

instance AsStrict Lazy.Text Lazy.Text Strict.Text where
  asStrict = Lazy
  fromStrict = Strict

instance AsStrict Strict.ByteString Lazy.ByteString Strict.ByteString where
  asLazy = Strict
  fromLazy = Lazy

instance AsStrict Lazy.ByteString Lazy.ByteString Strict.ByteString where
  asStrict = Lazy
  fromStrict = Strict

-- it's cheap to convert strict bytestring to lazy bytestring, and we must force the text to be a prism
utf8 :: (AsStrict b Lazy.ByteString Strict.ByteString, AsStrict t Lazy.Text Strict.Text) => Prism' b t
utf8 = prism' (fromLazy . Lazy.encodeUtf8 . asLazy) (fmap fromLazy . hush . Lazy.decodeUtf8' . asLazy)

exceptT :: Monad m => (e -> m b) -> (a -> m b) -> ExceptT e m a -> m b
exceptT f g = runExceptT >=> either f g

maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT b f = runMaybeT >=> maybe b f

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT a = maybeT a return

hush :: MonadPlus m => Either e a -> m a
hush = either (const mzero) return


hushT :: MonadPlus m => ExceptT e m a -> m a
hushT = runExceptT >=> hush

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) return

noteT :: MonadError e m => e -> MaybeT m a -> m a
noteT e = runMaybeT >=> note e

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

hoistEither :: MonadError e m => Either e a -> m a
hoistEither = either throwError return
