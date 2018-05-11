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
        (countBefore, countAfter) <- withDB $ do
          countBefore <- countUsers
          withStrategy Transaction $ createUser $ User "John" "Doe"
          countAfter <- countUsers
          return (countBefore, countAfter)

        countBefore `shouldBe` countAfter

    context "when the strategy is Truncation" $
      it "" $
        pending


withDB :: ReaderT Connection IO a -> IO a
withDB = bracket (connectPostgreSQL "dbname=dbcleaner") close . runReaderT

countUsers :: ReaderT Connection IO Int
countUsers = do
  c <- ask
  toCount <$> liftIO (query_ c "SELECT count(1) FROM users")
  where
    toCount []               = 0
    toCount (Only count : _) = count

createUser :: User -> ReaderT Connection IO ()
createUser user = do
  c <- ask
  void $ liftIO $ execute c "INSERT INTO users (first_name, last_name) VALUES (?, ?)" user
