{-# LANGUAGE RecordWildCards #-}

module Database.DBCleaner
  ( Adapter(..)
  , Strategy(..)
  , withStrategy
  ) where

import           Control.Monad.Catch

data Adapter m = Adapter
  { beginTransation     :: m ()
  , rollbackTransaction :: m ()
  , listTables          :: m [String]
  , truncateTables      :: [String] -> m ()
  }

data Strategy
  = Transaction
  | Truncation [String]

withStrategy
  :: MonadMask m
  => Adapter m
  -> Strategy
  -> m a
  -> m a
withStrategy c Transaction     = withTransaction c
withStrategy c (Truncation ts) = withTruncation c ts

withTransaction
  :: MonadMask m
  => Adapter m
  -> m a
  -> m a
withTransaction Adapter{..} = bracket_ beginTransation rollbackTransaction

withTruncation
  :: Monad m
  => Adapter m
  -> [String]
  -> m a
  -> m a
withTruncation Adapter{..} ts f = do
  listTables >>= truncateTables . filter (`notElem` ts)
  f
