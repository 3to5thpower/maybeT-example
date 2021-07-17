module Normal where

-- nothing to import
--

maybeVal :: Maybe Int
maybeVal = Just 3

newtype User = User {name :: String}

data Connection = Connection

sampleUser :: User
sampleUser = User "Tanaka"

connectDataBase :: Either String Connection
connectDataBase = Right Connection

getUser :: Connection -> Either String (Maybe User)
getUser connection = Right (Just sampleUser)

fetchResult :: Either String (Maybe User)
fetchResult = do
  connection <- connectDataBase
  getUser connection

addTaro :: Either String (Maybe User)
addTaro = do
  maybeUser <- fetchResult
  return $ do
    user <- maybeUser
    return $ User {name = name user ++ " Taro"}
