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
tablesQuery = mconcat [ "SELECT c.relname FROM pg_catalog.pg_class c"
                      , " LEFT JOIN pg_catalog.pg_namespace n"
                      , " ON c.relnamespace = n.oid"
                      , " WHERE c.relkind IN ('r', '')"
                      , " AND n.nspname <> 'pg_catalog'"
                      , " AND n.nspname <> 'information_schema'"
                      , " AND n.nspname !~ '^pg_toast'"
                      , " AND pg_catalog.pg_table_is_visible(c.oid)"
                      ]

truncateTable :: Connection -> Text -> IO ()
truncateTable c t = void $ execute c "TRUNCATE ? CASCADE" $ Only $ Identifier t
