module Database.DBCleaner.HDBC.PostgreSQLSpec where

import           Database.HDBC.PostgreSQL           (Connection)
import           Test.Hspec

import           Database.DBCleaner.HDBC.PostgreSQL (withPGConnection)
import           Database.DBCleaner.HDBC.TestHelper (User (..), createUser,
                                                     findAllUsers)
import           Database.DBCleaner.Types           (Strategy (..))

withTestConnection :: Strategy -> (Connection -> IO a) -> IO a
withTestConnection = withPGConnection "dbname=dbcleaner-hdbc"

spec :: Spec
spec =
  describe "withConnection" $ do
    context "when strategy is Transaction" $
      it "delete all records created inside the transaction" $ do
        withTestConnection Transaction $ \c -> do
            let user1 = User "user1" "user1@example.com"
            let user2 = User "user2" "user2@example.com"
            mapM_ (createUser c) [user1, user2]
            findAllUsers c >>= shouldMatchList [user1, user2]

        withTestConnection Transaction findAllUsers `shouldReturn` []

    context "when strategy is Truncation" $
      it "truncate all the tables of the schema" $ do
        withTestConnection Truncation $ \c -> do
          let user1 = User "user1" "user1@example.com"
          let user2 = User "user2" "user2@example.com"
          mapM_ (createUser c) [user1, user2]
          findAllUsers c >>= shouldMatchList [user1, user2]

        withTestConnection Truncation findAllUsers `shouldReturn` []
