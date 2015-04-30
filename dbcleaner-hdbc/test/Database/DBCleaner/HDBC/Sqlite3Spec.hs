module Database.DBCleaner.HDBC.Sqlite3Spec where

import           Database.HDBC.Sqlite3              (Connection)
import           Test.Hspec

import           Database.DBCleaner.HDBC.Sqlite3    (withS3Connection)
import           Database.DBCleaner.HDBC.TestHelper (User (..), createUser,
                                                     findAllUsers)
import           Database.DBCleaner.Types           (Strategy (..))

withTestConnection :: Strategy -> (Connection -> IO a) -> IO a
withTestConnection = withS3Connection "dbcleaner-hdbc.db"

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
      it "throws an exception" $
        withTestConnection Truncation findAllUsers `shouldThrow` errorCall "Sqlite3: strategy not supported"
