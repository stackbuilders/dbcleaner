module Database.DBCleaner.HDBC (withPGConnection, withS3Connection) where

import           Database.DBCleaner.HDBC.PostgreSQL (withPGConnection)
import           Database.DBCleaner.HDBC.Sqlite3    (withS3Connection)
