{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Database.DBCleaner.PostgreSQLSimpleSpec where

import           Control.Applicative
import           Control.Monad                       (void)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Test.Hspec

import           Database.DBCleaner.PostgreSQLSimple
import           Database.DBCleaner.Types            (Strategy (..))

data User = User
  { userName  :: String
  , userEmail :: String
  } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow User{..} = [toField userName, toField userEmail]

withConnection :: Strategy -> (Connection -> IO a) -> IO a
withConnection = withPGConnection "dbname=dbcleaner"

createUser :: Connection -> User -> IO ()
createUser c u = void $ execute c "INSERT INTO users (name, email) VALUES (?, ?)" u

listUsers :: Connection -> IO [User]
listUsers c = query_ c "SELECT name, email FROM users"

spec :: Spec
spec = do
  describe "withPGConnection" $ do
    context "when strategy is Transaction" $
      it "deletes all records create inside the transaction" $ do
        withConnection Transaction $ \c -> do
          let user1 = User "user1" "user1@example.com"
          let user2 = User "user2" "user2@example.com"
          mapM_ (createUser c) [user1, user2]
          listUsers c >>= shouldMatchList [user1, user2]

        withConnection Transaction listUsers `shouldReturn` []

    context "when strategy is Truncation" $
      it "truncates all the tables of the schema" $ do
        withConnection Truncation $ \c -> do
          let user1 = User "user1" "user1@example.com"
          let user2 = User "user2" "user2@example.com"
          mapM_ (createUser c) [user1, user2]
          listUsers c >>= shouldMatchList [user1, user2]

        withConnection Truncation listUsers `shouldReturn` []
