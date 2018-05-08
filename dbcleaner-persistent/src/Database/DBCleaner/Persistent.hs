module Database.DBCleaner.Persistent
  ( Strategy(..)
  , withFoo
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Database.DBCleaner
import           Database.Persist.Sql

withFoo
  :: (MonadIO m, MonadMask m)
  => Strategy
  -> SqlPersistT m a
  -> SqlPersistT m a
withFoo = withStrategy Adapter
  { beginTransation     = transactionSave
  , rollbackTransaction = transactionUndo
  , listTables          = undefined
  , truncateTables      = undefined
  }
