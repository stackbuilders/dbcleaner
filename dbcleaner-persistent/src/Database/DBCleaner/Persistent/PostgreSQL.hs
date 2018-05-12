{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.Persistent.PostgreSQL
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
  { adapterBeginTransaction    = transactionSave
  , adapterRollbackTransaction = transactionUndo
  , adapterListTables          = listTables
  , adapterTruncateTables      = truncateTables
  }

listTables :: MonadIO m => SqlPersistT m [String]
listTables = fmap unSingle <$> rawSql q []
  where
    q = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"

truncateTables :: MonadIO m => [String] -> SqlPersistT m ()
truncateTables = undefined
