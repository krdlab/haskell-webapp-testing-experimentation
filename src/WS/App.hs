{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module WS.App where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import Data.Time (getCurrentTimeZone, getCurrentTime, utcToLocalTime, LocalTime)
import Servant
import WS.Types as User
import WS.Cache as Cache
import WS.DB as DB
import WS.Mail as Mail
import Database.Relational.Query
import Data.Int (Int32)

type API =   "register" :> ReqBody '[JSON] RegForm :> Post '[JSON] User
        :<|> "login" :> ReqBody '[JSON] LoginForm :> Post '[] ()
        :<|> "users" :> Capture "userId" Int :> Get '[JSON] User

api :: Proxy API
api = Proxy

server :: Server API
server = registerH
    :<|> loginH
    :<|> getUserH
  where
    registerH f = lift $ register f
    loginH    f = lift $ login f
    getUserH  i = lift $ getUser i

register :: RegForm -> IO User      -- TODO: MonadMail m, MonadCache m, MonadDB m => ...
register form = do
    lt <- getCurrentLocalTime
    insertUser' (regName form) (regEmailAddress form) lt
    user' <- getUserByName (regName form)
    sendMail "admin@example.com" (emailAddress user') "register" (pack $ "registered at " ++ show lt)
    return user'

login :: LoginForm -> IO ()
login form = do
    lt <- getCurrentLocalTime
    let name = loginName form
    updateUser' name lt
    user' <- getUserByName name
    sendMail "admin@example.com" (emailAddress user') "login" (pack $ "logged-in at " ++ show lt)

getUser :: Int -> IO User
getUser uid = do
    user' <- getUserCache uid
    case user' of
        Just user -> return user
        Nothing   -> do
            u <- getUserById uid
            setUserCache u
            return u

getUserCache :: Int -> IO (Maybe User)
getUserCache key = Cache.get (BS.pack . show $ key)

setUserCache :: User -> IO ()
setUserCache u = Cache.set (BS.pack . show $ User.id u) u

insertUser' :: Text -> Text -> LocalTime -> IO Integer
insertUser' name addr time = insert (typedInsert tableOfUser piUser) ins
  where
    ins = InsertUser { insName = name
                     , insEmailAddress = addr
                     , insCreatedAt = time
                     , insLastLoggedinAt = time
                     }

updateUser' :: Text -> LocalTime -> IO Integer
updateUser' name time = update (typedUpdate tableOfUser target) ()
  where
    target = updateTarget $ \proj -> do
        User.lastLoggedinAt' <-# value time
        wheres $ proj ! User.name' .=. value name

getUserById :: Int -> IO User
getUserById i = head <$> select (relationalQuery rel) (fromIntegral i)  -- TODO: empty list
  where
    rel = relation' . placeholder $ \ph -> do
        u <- query user
        wheres $ u ! User.id' .=. ph
        return u

getUserByName :: Text -> IO User
getUserByName n = head <$> select (relationalQuery rel) n               -- TODO: empty list
  where
    rel = relation' . placeholder $ \ph -> do
        u <- query user
        wheres $ u ! User.name' .=. ph
        return u

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
