module Database.DBCleanerSpec where

import           Control.Monad.Catch
import           Control.Monad.Writer
import           Database.DBCleaner
import           Test.Hspec

spec :: Spec
spec =
  describe "withAdapter" $ do
    let adapter = Adapter
          { adapterBeginTransaction    = tell ["begin"]
          , adapterRollbackTransaction = tell ["rollback"]
          , adapterListTables          = return ["table1", "table2"]
          , adapterTruncateTables      = tell
          }

    context "when the strategy is Transaction" $ do
      it "rollbacks the transaction" $ do
        output <- execWriterT $
          withAdapter adapter Transaction $ tell ["action"]
        output `shouldBe` ["begin", "action", "rollback"]

    context "when the strategy is Truncation" $ do
      context "and there are tables excluded" $
        it "truncates all tables except the ones excluded" $ do
          output <- execWriterT $
            withAdapter adapter (Truncation ["table1"]) $ tell ["action"]
          output `shouldBe` ["table2", "action"]

      context "and there are no tables excluded" $
        it "truncates all tables" $ do
          output <- execWriterT $
            withAdapter adapter (Truncation []) $ tell ["action"]
          output `shouldBe` ["table1", "table2", "action"]
