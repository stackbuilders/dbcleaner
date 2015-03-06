{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.DBCleaner.PostgreSQLSimpleSpec where

import Control.Applicative
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Test.Hspec

import Database.DBCleaner.PostgreSQLSimple
import Database.DBCleaner.Types

data User = User
  { userId   :: Maybe Integer
  , userName :: String
  } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow User{..} = [toField  userName]

createUser :: Connection -> User -> IO User
createUser c u = foo u `fmap` query c "INSERT INTO users (name) VALUES (?) RETURNING id" u

foo :: User -> [Only Integer] -> User
foo u (x:_) = u{ userId = Just $ fromOnly x }

listUsers :: Connection -> IO [User]
listUsers c = query_ c "SELECT id, name FROM users"

withConnection :: Strategy -> (Connection -> IO a) -> IO a
withConnection = withPostgreSQLSimpleConnection "dbname=dbcleaner"

spec :: Spec
spec = do
  describe "withPostgreSQLSimpleConnection" $ do
    it "rollbacks the transaction" $ do
      withConnection Transaction $ \c -> do
        users <- mapM (createUser c . User Nothing) ["User 1", "User 2"]
        listUsers c >>= shouldMatchList users

      withConnection Transaction listUsers `shouldReturn` []

    it "rollbacks the transaction" $ do
      withConnection Transaction createUsers `shouldThrow` anyException
      withConnection Transaction listUsers `shouldReturn` []
      where
        createUsers c = mapM_ (createUser c . User Nothing) ["User 1", "User 1"]
