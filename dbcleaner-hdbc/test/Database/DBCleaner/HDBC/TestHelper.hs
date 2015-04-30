module Database.DBCleaner.HDBC.TestHelper where

import           Control.Monad (void)
import           Data.Maybe    (mapMaybe)
import           Database.HDBC (IConnection (..), SqlValue (..), fromSql,
                                quickQuery)

data User = User { userName :: String, userEmail :: String } deriving (Show, Eq)

createUser :: IConnection a => a -> User -> IO ()
createUser c = createUser' c . userToValues

createUser' :: IConnection a => a -> [SqlValue] -> IO ()
createUser' c vs = void $ quickQuery c "INSERT INTO users (name, email) VALUES (?, ?)" vs

userToValues :: User -> [SqlValue]
userToValues u = [SqlString $ userName u, SqlString $ userEmail u]

findAllUsers :: IConnection a => a -> IO [User]
findAllUsers c = mapMaybe valuesToUser `fmap` findAllUsers' c

findAllUsers' :: IConnection a => a -> IO [[SqlValue]]
findAllUsers' c = quickQuery c "SELECT name, email FROM users" []

valuesToUser :: [SqlValue] -> Maybe User
valuesToUser [n, e] = Just $ User (fromSql n) (fromSql e)
valuesToUser _      = Nothing
