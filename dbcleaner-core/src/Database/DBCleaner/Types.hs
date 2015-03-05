module Database.DBCleaner.Types where

data Strategy = Transaction | Truncation

class Connection a where
  begin    :: a -> Strategy -> IO ()
  rollback :: a -> Strategy -> IO ()
