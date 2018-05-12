{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.PostgreSQL.SimpleSpec
  ( spec
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Database.DBCleaner.PostgreSQL.Simple
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Debug.Trace
import           GHC.Generics
import           Test.Hspec

data User = User
  { userFirstName :: String
  , userLastName  :: String
  } deriving Generic

instance ToRow User

spec :: Spec
spec = beforeAll (withDB createTableUsers) $ afterAll_ (withDB dropTableUsers) $
  describe "withStrategy" $ do
    context "when the strategy is Transaction" $ do
      context "and there is an error during the operation" $
        it "rollbacks the transaction" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            countAfter <- handleAll (const countUsers) $
              withStrategy Transaction $ do
                createUser $ User "John" "Doe"
                fail "Boom!"

            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter

      context "and there is no error during the operation" $
        it "rollbacks the transaction" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            withStrategy Transaction $ createUser $ User "John" "Doe"
            countAfter <- countUsers
            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter

    context "when the strategy is Truncation" $ do
      context "and there is an error during the operation" $
        it "truncates all tables" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            countAfter <- handleAll (const countUsers) $
              withStrategy (Truncation []) $ do
                createUser $ User "John" "Doe"
                fail "Boom!"

            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter

      context "and there is no error during the operation" $
        it "truncates all tables" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            withStrategy (Truncation []) $ createUser $ User "John" "Doe"
            countAfter <- countUsers
            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter


withDB :: ReaderT Connection IO a -> IO a
withDB = bracket (connectPostgreSQL "dbname=dbcleaner") close . runReaderT

createTableUsers :: ReaderT Connection IO ()
createTableUsers = do
  c <- ask
  void $ liftIO $ execute_ c $ mconcat
    [ "CREATE TABLE users ("
    , "  id serial PRIMARY KEY,"
    , "  first_name varchar NOT NULL,"
    , "  last_name varchar NOT NULL"
    , ")"
    ]

dropTableUsers :: ReaderT Connection IO ()
dropTableUsers = do
  c <- ask
  void $ liftIO $ execute_ c "DROP TABLE users"

countUsers :: ReaderT Connection IO Int
countUsers = do
  c <- ask
  toCount <$> liftIO (query_ c "SELECT count(1) FROM users")
  where
    toCount []           = 0
    toCount (Only x : _) = x

createUser :: User -> ReaderT Connection IO ()
createUser user = do
  c <- ask
  void $ liftIO $ execute c "INSERT INTO users (first_name, last_name) VALUES (?, ?)" user
