{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
module WS.App where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, MonadCatch, throwM, catch)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Either as E
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import Data.Time (getCurrentTimeZone, getCurrentTime, utcToLocalTime, LocalTime)
import Data.Typeable (Typeable)
import Database.Relational.Query
import Servant
import WS.Cache as Cache
import WS.DB as DB
import WS.Mail as Mail
import qualified WS.Types as User
import WS.Types (User, user, tableOfUser, InsertUser(..), piUser, RegForm(..), LoginForm(..))

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
    registerH f = lift (register f) `catch` errorH
    loginH    f = lift (login f)    `catch` errorH
    getUserH  i = lift (getUser i)  `catch` errorH
    -- errorH :: Monad m => AppException -> E.EitherT ServantErr m a
    errorH NotFound = E.left err404

-- handlers

adminAddress :: Text
adminAddress = "admin@example.com"

data AppException = NotFound deriving (Show, Typeable)
instance Exception AppException

class (MonadCatch m, MonadMail m, MonadCache m, MonadDB m) => App m where
    getCurrentLocalTime :: m LocalTime

instance App IO where
    getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

register :: App m => RegForm -> m User
register form = do
    lt <- getCurrentLocalTime
    _ <- insertUser' (regName form) (regEmailAddress form) lt
    user' <- getUserByName (regName form)
    sendMail
        adminAddress
        (User.emailAddress user')
        "register"
        (pack $ "registered at " ++ show lt)
    return user'

login :: App m => LoginForm -> m ()
login form = do
    lt <- getCurrentLocalTime
    let name' = loginName form
    _ <- updateUser' name' lt
    user' <- getUserByName name'
    sendMail
        adminAddress
        (User.emailAddress user')
        "login"
        (pack $ "logged-in at " ++ show lt)

getUser :: App m => Int -> m User
getUser userId = do
    u <- getUserCache userId
    case u of
        Just u' -> return u'
        Nothing   -> do
            u' <- getUserById userId
            setUserCache u'
            return u'

-- operations

getUserCache :: MonadCache m => Int -> m (Maybe User)
getUserCache pk = Cache.get (BS.pack . show $ pk)

setUserCache :: MonadCache m => User -> m ()
setUserCache u = Cache.set (BS.pack . show $ User.id u) u

insertUser' :: MonadDB m => Text -> Text -> LocalTime -> m Integer
insertUser' name addr time = insert (typedInsert tableOfUser piUser) ins
  where
    ins = InsertUser { insName = name
                     , insEmailAddress = addr
                     , insCreatedAt = time
                     , insLastLoggedinAt = time
                     }

updateUser' :: MonadDB m => Text -> LocalTime -> m Integer
updateUser' name time = update (typedUpdate tableOfUser target) ()
  where
    target = updateTarget $ \proj -> do
        User.lastLoggedinAt' <-# value time
        wheres $ proj ! User.name' .=. value name

getUserById :: (MonadThrow m, MonadDB m) => Int -> m User
getUserById pk = do
    res <- select (relationalQuery rel) (fromIntegral pk)
    if null res
        then throwM NotFound
        else return $ head res
  where
    rel = relation' . placeholder $ \ph -> do
        u <- query user
        wheres $ u ! User.id' .=. ph
        return u

getUserByName :: (MonadThrow m, MonadDB m) => Text -> m User
getUserByName name = do
    res <- select (relationalQuery rel) name
    if null res
        then throwM NotFound
        else return $ head res
  where
    rel = relation' . placeholder $ \ph -> do
        u <- query user
        wheres $ u ! User.name' .=. ph
        return u
