{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.PostgreSQL.SimpleSpec
  ( spec
  ) where

import           Control.Exception
import           Control.Monad.Reader
import           Database.DBCleaner.PostgreSQL.Simple
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Test.Hspec

data User = User
  { userFirstName :: String
  , userLastName  :: String
  } deriving Generic

instance ToRow User

spec :: Spec
spec =
  describe "withStrategy" $ do
    context "when the strategy is Transaction" $
      it "" $ do
        withDB $ withStrategy Transaction $ do
          c <- ask
          undefined $ execute "" ()

          undefined

        pending

    context "when the strategy is Truncation" $
      it "" $
        pending


withDB :: ReaderT Connection IO a -> IO a
withDB = bracket (connectPostgreSQL "dbname=dbcleaner") close . runReaderT
