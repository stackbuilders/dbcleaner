{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.DBCleaner.Persistent.PostgreSQLSpec
  ( spec
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Database.DBCleaner.Persistent.PostgreSQL
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Database.Persist.TH
import           Test.Hspec

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  firstName String
  lastName String
|]

spec :: Spec
spec = beforeAll (withDB createTableUsers) $ afterAll_ (withDB dropTableUsers) $ do
  describe "withStrategy" $ do
    context "when the strategy is Transaction" $ do
      context "and there is an error during the operation" $
        it "rollbacks the Transaction" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            countAfter <- handleAll (const countUsers) $
              withStrategy Transaction $ do
                insert_ $ User "John" "Doe"
                fail "Boom!"

            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter

      context "and there is no error during the operation" $
        it "rollbacks the transaction" $ do
          (countBefore, countAfter) <- withDB $ do
            countBefore <- countUsers
            withStrategy Transaction $ insert_ $ User "John" "Doe"
            countAfter <- countUsers
            return (countBefore, countAfter)

          countBefore `shouldBe` countAfter

    context "when the strategy is Truncation" $ do
      it "truncates all tables" $ do
        (countBefore, countAfter) <- withDB $ do
          countBefore <- countUsers
          withStrategy (Truncation []) $ insert_ $ User "John" "Doe"
          countAfter <- countUsers
          return (countBefore, countAfter)

        countBefore `shouldBe` countAfter


withDB :: SqlPersistT (NoLoggingT IO) a -> IO a
withDB = runNoLoggingT . withPostgresqlConn "dbname=dbcleaner" . runSqlConn

createTableUsers :: SqlPersistT (NoLoggingT IO) ()
createTableUsers = void $ runMigrationSilent migrateAll

dropTableUsers :: SqlPersistT (NoLoggingT IO) ()
dropTableUsers = rawExecute "DROP TABLE users" []

countUsers :: MonadIO m => SqlPersistT m Int
countUsers = count ([] :: [Filter User])
