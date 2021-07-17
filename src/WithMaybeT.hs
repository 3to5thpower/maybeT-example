module WithMaybeT where

import Control.Monad.Trans (MonadTrans (lift))
import MaybeT (MaybeT (MaybeT))

maybeVal :: Maybe Int
maybeVal = Just 3

newtype User = User {name :: String}

data Connection = Connection

sampleUser :: User
sampleUser = User "Tanaka"

connectDataBase :: Either String Connection
connectDataBase = Right Connection

getUser :: Connection -> MaybeT (Either String) User
getUser _ = MaybeT $ Right (Just sampleUser)

fetchResult :: MaybeT (Either String) User
fetchResult = do
  connection <- lift connectDataBase
  getUser connection

addTaro :: MaybeT (Either String) User
addTaro = do
  user <- fetchResult
  return $ User {name = name user ++ " Taro"}
