module Database.DBCleanerSpec where

import           Control.Monad.Catch
import           Control.Monad.Writer
import           Database.DBCleaner
import           Test.Hspec

spec :: Spec
spec =
  describe "withStrategy" $ do
    let adapter = Adapter
          { beginTransation     = tell ["begin"]
          , rollbackTransaction = tell ["rollback"]
          , listTables          = return ["table1", "table2"]
          , truncateTables      = tell
          }

    context "when the strategy is Transaction" $
      it "" $ do
        output <- execWriterT $
          withStrategy adapter Transaction $ tell ["action"]
        output `shouldBe` ["begin", "action", "rollback"]

    context "when the strategy is Truncation" $ do
      it "" $ do
        output <- execWriterT $
          withStrategy adapter (Truncation []) $ tell ["action"]
        output `shouldBe` ["table1", "table2", "action"]

      it "" $ do
        output <- execWriterT $
          withStrategy adapter (Truncation ["table1"]) $ tell ["action"]
        output `shouldBe` ["table2", "action"]
