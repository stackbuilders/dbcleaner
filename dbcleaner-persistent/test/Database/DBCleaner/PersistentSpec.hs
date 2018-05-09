{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.DBCleaner.PersistentSpec where

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.List (length)
import           Database.DBCleaner.Persistent
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Test.Hspec

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  firstName String
  lastName String
|]

spec :: Spec
spec = do
  describe "withStrategy" $
    context "when the strategy is Transaction" $
      it "" $ do
        countUsers <- withDB $ withStrategy Transaction $ do
          runMigration migrateAll
          insert $ User "Napoleon" "Dynamite"
          count ([] :: [Filter User])

        countUsers `shouldBe` 0

withDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
withDB = runSqlite ":memory:"
