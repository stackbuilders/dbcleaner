module Database.DBCleaner.PostgreSQLSimple.Types where

import qualified Database.PostgreSQL.Simple as PG

import Database.DBCleaner.Types

instance GenericConnection PG.Connection where
  begin c Transaction = PG.begin c
  begin c _           = undefined

  rollback c Transaction = PG.rollback c
  rollback c _           = undefined
