module Database.DBCleaner.PostgreSQLSimple where

import           Data.ByteString
import           Database.PostgreSQL.Simple

import           Database.DBCleaner
import           Database.DBCleaner.PostgreSQLSimple.Types
import           Database.DBCleaner.Types

withPGConnection :: ByteString -> Strategy -> (Connection -> IO a) -> IO a
withPGConnection cs s f = connectPostgreSQL cs >>= \c -> withConnection c s f
