{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.DBCleaner.HDBC where

import           Database.DBCleaner (GenericConnection (..), Strategy (..))
import           Database.HDBC      (IConnection (..))

class IConnection a => HDBCConnection a where
  beginTransaction :: a -> IO ()
  truncateTable :: a -> String -> IO ()

instance HDBCConnection a => GenericConnection a where
  before c Transaction = beginTransaction c
  before c Truncation = getTables c >>= mapM_ (truncateTable c)

  after c Transaction = rollback c
  after c Truncation = before c Truncation
