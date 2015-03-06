module Database.DBCleaner.PostgreSQLSimple where

import Data.ByteString
import Database.PostgreSQL.Simple

import Database.DBCleaner
import Database.DBCleaner.PostgreSQLSimple.Types
import Database.DBCleaner.Types

withPostgreSQLSimpleConnection :: ByteString -> Strategy -> (Connection -> IO a) -> IO a
withPostgreSQLSimpleConnection cs s f = connectPostgreSQL cs >>= \c -> withConnection c s f
