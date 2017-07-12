{-# LANGUAGE ForeignFunctionInterface #-}
module Sqlite.Ext.Register where

import Foreign.Ptr

import Database.SQLite3.Bindings.Types
import Database.SQLite3.Direct

foreign import ccall "RegisterExtensionFunctions"
  register_internal :: Ptr CDatabase -> IO Int

registerExtensionFunctions :: Database -> IO Bool
registerExtensionFunctions (Database cdb) = (== 0) <$> register_internal cdb
