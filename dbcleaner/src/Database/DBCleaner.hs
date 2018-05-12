{-# LANGUAGE RecordWildCards #-}

module Database.DBCleaner
  ( Adapter(..)
  , Strategy(..)
  , withAdapter
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Debug.Trace

data Adapter m = Adapter
  { adapterBeginTransaction    :: m ()
  , adapterRollbackTransaction :: m ()
  , adapterListTables          :: m [String]
  , adapterTruncateTables      :: [String] -> m ()
  }

data Strategy
  = Transaction
  | Truncation [String]

withAdapter
  :: MonadMask m
  => Adapter m
  -> Strategy
  -> m a
  -> m a
withAdapter c Transaction     = withTransaction c
withAdapter c (Truncation ts) = withTruncation c ts

withTransaction
  :: MonadMask m
  => Adapter m
  -> m a
  -> m a
withTransaction Adapter{..} =
  bracket_ adapterBeginTransaction adapterRollbackTransaction

withTruncation
  :: Monad m
  => Adapter m
  -> [String]
  -> m a
  -> m a
withTruncation Adapter{..} ts f = do
  tables <- filter (`notElem` ts) <$> adapterListTables
  unless (null tables) $ adapterTruncateTables tables
  f
