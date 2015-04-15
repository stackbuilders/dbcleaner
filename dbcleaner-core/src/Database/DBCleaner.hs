module Database.DBCleaner where

import           Control.Exception (bracket)

data Strategy = Transaction | Truncation

class GenericConnection a where
  before :: a -> Strategy -> IO ()
  after :: a -> Strategy -> IO ()

withConnection :: GenericConnection a => a -> Strategy -> (a -> IO b) -> IO b
withConnection c s = bracket (before c s >> return c) (`after` s)
