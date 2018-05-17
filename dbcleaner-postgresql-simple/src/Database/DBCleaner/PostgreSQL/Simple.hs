{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.PostgreSQL.Simple
  ( Strategy(..)
  , withStrategy
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List
import           Data.String
import           Database.DBCleaner
import           Database.PostgreSQL.Simple

withStrategy
  :: (MonadIO m, MonadMask m)
  => Strategy
  -> ReaderT Connection m a
  -> ReaderT Connection m a
withStrategy = withAdapter Adapter
  { adapterBeginTransaction    = liftPS begin
  , adapterRollbackTransaction = liftPS rollback
  , adapterListTables          = liftPS listTables
  , adapterTruncateTables      = liftPS . truncateTables
  }

liftPS
  :: MonadIO m
  => (Connection -> IO a)
  -> ReaderT Connection m a
liftPS f = ask >>= liftIO . f

listTables :: Connection -> IO [String]
listTables c = fmap fromOnly <$> query_ c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"

truncateTables :: [String] -> Connection -> IO ()
truncateTables tables c = void $ execute_ c $ mconcat
  [ "TRUNCATE TABLE "
  , fromString $ intercalate ", " tables
  , " CASCADE"
  ]
