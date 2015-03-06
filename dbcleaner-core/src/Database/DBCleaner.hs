module Database.DBCleaner where

import Control.Exception (bracket)

import Database.DBCleaner.Types (GenericConnection(..), Strategy)

withConnection :: GenericConnection a => a -> Strategy -> (a -> IO b) -> IO b
withConnection c s = bracket (begin c s >> return c) (`rollback` s)
