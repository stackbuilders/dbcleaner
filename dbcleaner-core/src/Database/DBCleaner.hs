module Database.DBCleaner where

import Database.DBCleaner.Types

withConnection :: Connection a => a -> Strategy -> (a -> IO b) -> IO b
withConnection = undefined

beginTransaction :: Connection a => a -> IO a
beginTransaction = undefined

rollbackTransaction :: Connection a => a -> IO ()
rollbackTransaction = undefined
