{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.DBCleaner.HDBC.Sqlite3 (withS3Connection) where

import           Database.HDBC            (IConnection (..))
import           Database.HDBC.Sqlite3    (Connection, connectSqlite3)

import           Database.DBCleaner       (withConnection)
import           Database.DBCleaner.Types (GenericConnection (..),
                                           Strategy (..))

instance GenericConnection Connection where
  -- HDBC-sqlite3 already begins a transaction by default
  -- https://github.com/hdbc/hdbc-sqlite3/blob/master/Database/HDBC/Sqlite3/Connection.hs#L62
  before _ Transaction = return ()
  before _ Truncation = error "Sqlite3: strategy not supported"

  after c Transaction = rollback c
  after c Truncation = before c Truncation

withS3Connection :: FilePath -> Strategy -> (Connection -> IO a) -> IO a
withS3Connection p s f = connectSqlite3 p >>= \c -> withConnection c s f
