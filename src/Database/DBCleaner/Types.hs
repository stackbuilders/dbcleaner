module Database.DBCleaner.Types where

data Strategy = Transaction | Truncation

class GenericConnection a where
  before :: a -> Strategy -> IO ()
  after :: a -> Strategy -> IO ()
