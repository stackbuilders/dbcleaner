{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.DBCleaner.HDBC.PostgreSQL (Connection(..), withPGConnection) where

import           Control.Monad            (void)
import           Data.Maybe               (mapMaybe)
import           Database.HDBC            (IConnection (..), SqlValue (..),
                                           fromSql, quickQuery)
import           Database.HDBC.PostgreSQL (Connection, begin, connectPostgreSQL)

import           Database.DBCleaner       (withConnection)
import           Database.DBCleaner.Types (GenericConnection (..),
                                           Strategy (..))

instance GenericConnection Connection where
  before c Transaction = begin c
  before c Truncation = listTables c >>= mapM_ (truncateTable c)

  after c Transaction = rollback c
  after c Truncation = before c Truncation

listTables :: Connection -> IO [String]
listTables c = mapMaybe valuesToString `fmap` quickQuery c tablesQuery []

valuesToString :: [SqlValue] -> Maybe String
valuesToString []    = Nothing
valuesToString (v:_) = Just $ fromSql v

tablesQuery :: String
tablesQuery = concat [ "SELECT c.relname FROM pg_catalog.pg_class c"
                     , " LEFT JOIN pg_catalog.pg_namespace n"
                     , " ON c.relnamespace = n.oid"
                     , " WHERE c.relkind IN ('r', '')"
                     , " AND n.nspname <> 'pg_catalog'"
                     , " AND n.nspname <> 'information_schema'"
                     , " AND n.nspname !~ '^pg_toast'"
                     , " AND pg_catalog.pg_table_is_visible(c.oid)"
                     ]

truncateTable :: Connection -> String -> IO ()
truncateTable c t = void $ quickQuery c (concat ["TRUNCATE ", t, " CASCADE"]) []

withPGConnection :: String -> Strategy -> (Connection -> IO a) -> IO a
withPGConnection cs s f = connectPostgreSQL cs >>= \c -> withConnection c s f
