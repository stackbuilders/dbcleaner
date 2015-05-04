{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.PostgreSQLSimple.Types where

import           Control.Monad                    (void)
import           Data.Monoid                      (mconcat)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, Only (..), Query,
                                                   begin, execute, query_,
                                                   rollback)
import           Database.PostgreSQL.Simple.Types (Identifier (..))

import           Database.DBCleaner.Types         (GenericConnection (..),
                                                   Strategy (..))

instance GenericConnection Connection where
  before c Transaction = begin c
  before c Truncation  = getTables c >>= mapM_ (truncateTable c)

  after c Transaction = rollback c
  after c Truncation  = before c Truncation

getTables :: Connection -> IO [Text]
getTables c = map fromOnly `fmap` query_ c tablesQuery

tablesQuery :: Query

