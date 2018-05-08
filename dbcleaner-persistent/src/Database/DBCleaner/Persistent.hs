module Database.DBCleaner.Persistent
  ( Strategy(..)
  , withStrategy
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Database.DBCleaner
import           Database.Persist.Sql

withStrategy
  :: (MonadIO m, MonadMask m)
  => Strategy
  -> SqlPersistT m a
  -> SqlPersistT m a
withStrategy = withAdapter Adapter
  { beginTransation     = transactionSave
  , rollbackTransaction = transactionUndo
  , listTables          = undefined
  , truncateTables      = undefined
  }
