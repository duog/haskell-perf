{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module MeasureGhc.Prelude
  ( module MeasureGhc.Prelude
  , module E
  ) where

import Control.Applicative as E
import Control.Exception.Safe as E
import Control.Lens as E
import Control.Monad as E
import Control.Monad.IO.Class as E
import Control.Monad.State.Class as E
import Control.Monad.Writer.Class as E
import Control.Monad.Reader as E
import Control.Monad.Trans.Maybe as E
import Control.Error.Util as E hiding ((??), tryIO)
import Data.Either as E (partitionEithers)
import Data.ByteString as E (ByteString)
import Data.Bitraversable as E
import Data.Foldable as E
import Data.List as E hiding (uncons)
import Data.List.Lens as E
import Data.Text as E (Text)
import Data.Map as E (Map)
import Data.Maybe as E
import Data.Semigroup as E
import Data.Sequence as E (Seq)
import Data.Sequence.Lens as E hiding (sliced)
import Data.Set as E (Set)
import Data.Set.Lens as E
import Data.Traversable as E
import GHC.Stack as E
import Numeric as E
import Data.MemoTrie as E
import Data.Vector.Generic.Lens as E hiding (sliced)
import Data.Vector.Generic as E (Vector)
import Data.Function as E
import Data.Proxy as E
import Control.Monad.Zip as E
import qualified Data.Text as T
import qualified Data.Sequence.Lens as Seq
import qualified Data.Vector.Generic.Lens as Vector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

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

type UVector = U.Vector
type BVector = V.Vector
