module Database.DBCleaner.PostgreSQL.Simple
  ( Strategy(..)
  , withStrategy
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Database.DBCleaner
import           Database.PostgreSQL.Simple

withStrategy
  :: MonadMask m
  => Strategy
  -> ReaderT Connection m a
  -> ReaderT Connection m a
withStrategy = withAdapter Adapter
  { beginTransation     = liftPS begin
  , rollbackTransaction = liftPS rollback
  , listTables          = undefined
  , truncateTables      = undefined
  }

liftPS :: (Connection -> IO a) -> ReaderT Connection m a
liftPS = undefined
