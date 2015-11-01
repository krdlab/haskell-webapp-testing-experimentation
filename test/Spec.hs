{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative (Applicative, (<$>), (<*))
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State as S
import Control.Monad.State (StateT, runStateT, MonadState)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.HDBC (commit)
import Database.HDBC.Record (runQuery', runInsert, runUpdate)
import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Network.HTTP.Types (methodPost, hContentType)
import Network.Mail.Mime (Mail(..), Address(..))
import Network.Mail.SMTP (simpleMail, plainTextPart)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Servant
import qualified WS.App as WS
import qualified WS.Cache as WS
import qualified WS.DB as WS
import qualified WS.Mail as WS
import qualified WS.Types as WS

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    registerSpec
--    postSpec
--    getSpec

-- Mock ver.

registerSpec :: Spec
registerSpec =
    describe "POST /register" $
        it "mock registration" $ do
            curr <- getPOSIXTime
            let form = WS.RegForm (pack $ "user-" ++ show curr) (pack $ "user-" ++ show curr ++ "@localhost")

            (res, state) <- runMock (WS.register form) initState

            WS.name res `shouldBe` WS.regName form
            emails state `shouldNotSatisfy` null
            (addressEmail . head . mailTo . head . emails $ state) `shouldBe` WS.regEmailAddress form
            history state `shouldBe` ["DB.insert", "DB.select", "Mail.sendMail"]

data MockAppState = MockAppState
    { currentTime :: LocalTime
    , cache :: Map.Map ByteString ByteString
    , emails :: [Mail]
    , history :: [String]
    }

initState :: MockAppState
initState = MockAppState
    { currentTime = LocalTime (ModifiedJulianDay 57327) (TimeOfDay 9 00 00)
    , cache = Map.empty
    , emails = []
    , history = []
    }

saveHistory :: String -> MockAppState -> MockAppState
saveHistory m s = s { history = history s ++ [m] }

newtype MockApp m a = MockApp
    { app :: StateT MockAppState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)

deriving instance (Functor m, MonadCatch m) => MonadState MockAppState (MockApp m)
deriving instance (Functor m, MonadCatch m, MonadIO m) => MonadIO (MockApp m)

runMock :: Serialize a => MockApp m a -> MockAppState -> m (a, MockAppState)
runMock (MockApp ap) = runStateT ap

instance (Functor m, MonadCatch m) => WS.MonadCache (MockApp m) where
    get k = do
        S.modify $ saveHistory "Cache.get"
        s <- S.get
        return $ decode' <$> Map.lookup k (cache s)
      where
        decode' bs = case Serialize.decode bs of
                         Right v -> v
                         Left  e -> error e
    set k v = do
        S.modify $ saveHistory "Cache.set"
        S.modify (\s -> s { cache = Map.insert k (Serialize.encode v) (cache s) })

instance (Functor m, MonadCatch m) => WS.MonadMail (MockApp m) where
    sendMail from to sub body = do
        S.modify $ saveHistory "Mail.sendMail"
        S.modify (\s -> s { emails = mail : emails s })
      where
        mail  = simpleMail from' [to'] [] [] sub [plainTextPart (fromStrict body)]
        from' = Address Nothing from
        to'   = Address Nothing to

connect :: IO Connection
connect = connectSqlite3 "test.db"

instance (Functor m, MonadCatch m, MonadIO m) => WS.MonadDB (MockApp m) where
    select q ps = do
        S.modify $ saveHistory "DB.select"
        liftIO $ withConnectionIO connect $ \conn -> runQuery' conn q ps
    insert i ps = do
        S.modify $ saveHistory "DB.insert"
        liftIO $ withConnectionIO connect $ \conn -> runInsert conn i ps <* commit conn
    update u ps = do
        S.modify $ saveHistory "DB.update"
        liftIO $ withConnectionIO connect $ \conn -> runUpdate conn u ps <* commit conn

instance (Functor m, MonadCatch m, MonadIO m) => WS.App (MockApp m) where
    getCurrentLocalTime = do
        s <- S.get
        return $ currentTime s

-- IO ver.

withServer :: SpecWith Application -> Spec
withServer = with (return $ serve WS.api WS.server)

postSpec :: Spec
postSpec =
    describe "POST /login" . withServer $ do
        let postJson p = Test.Hspec.Wai.request methodPost p [
                  (hContentType, "application/json;charset=utf-8")
                ]
        it "user found" $ do
            let form = WS.LoginForm "tester"
            postJson "/login" (encode form) `shouldRespondWith` 204
        it "user not found" $ do
            let form = WS.LoginForm "tester1"
            postJson "/login" (encode form) `shouldRespondWith` 404

getSpec :: Spec
getSpec =
    describe "GET /users/:id" . withServer $ do
        it "user found" $
            get "/users/2" `shouldRespondWith` 200
        it "user not found" $
            get "/users/0" `shouldRespondWith` 404
