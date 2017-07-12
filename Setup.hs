import Distribution.Simple
import Sqlite.Ext.Setup

main :: IO ()
main = defaultMainWithHooks
  simpleUserHooks
    { confHook = sqliteExtensionFunctionsConfHook "data/sqliteextensionslib"
    }
