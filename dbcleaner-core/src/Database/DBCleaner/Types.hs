module Database.DBCleaner.Types where

data Strategy = Transaction | Truncation
                deriving (Show, Eq)

class GenericConnection a where
  begin    :: a -> Strategy -> IO ()
  rollback :: a -> Strategy -> IO ()
