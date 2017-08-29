{-# LANGUAGE FlexibleInstances #-}
module MeasureGhc.JSON
  ( module MeasureGhc.JSON
  , module E
  ) where

import Data.Aeson as E
import Data.Aeson.Lens as E

import qualified Data.Yaml as Y
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text as Strict
import Data.Text.Encoding as Strict
import Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding as Lazy
import Control.Lens
import Data.Text.Lens
import Data.Aeson.Parser
import Data.Attoparsec.ByteString.Lazy

-- The below is taken from lens-aeson
strictUtf8 :: Iso' String Strict.ByteString
strictUtf8 = packed . strictTextUtf8

strictTextUtf8 :: Iso' Strict.Text Strict.ByteString
strictTextUtf8 = iso Strict.encodeUtf8 Strict.decodeUtf8

lazyTextUtf8 :: Iso' Lazy.Text Lazy.ByteString
lazyTextUtf8 = iso Lazy.encodeUtf8 Lazy.decodeUtf8

class AsYAML t where
  -- | '_YAML is a 'Prism' from something containing YAML to something encoded in that structure
  _YAML :: (FromJSON a, ToJSON a) => Prism' t a

instance AsYAML Strict.ByteString where
  _YAML = prism' Y.encode decodeValue
    where
      decodeValue :: (FromJSON a) => Strict.ByteString -> Maybe a
      decodeValue s = Y.decode s >>= \x -> case fromJSON x of
        Success v -> Just v
        _         -> Nothing
  {-# INLINE _YAML #-}

instance AsYAML Lazy.ByteString where
  _YAML = strict._YAML
  {-# INLINE _YAML #-}

instance AsYAML String where
  _YAML = strictUtf8._YAML
  {-# INLINE _YAML #-}

instance AsYAML Strict.Text where
  _YAML = strictTextUtf8._YAML
  {-# INLINE _YAML #-}

instance AsYAML Lazy.Text where
  _YAML = lazyTextUtf8._YAML
  {-# INLINE _YAML #-}

instance AsYAML Value where
  _YAML = _JSON
  {-# INLINE _YAML #-}
