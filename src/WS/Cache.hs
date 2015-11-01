{-# LANGUAGE OverloadedStrings #-}
module WS.Cache where

import Control.Applicative (Applicative)
import Data.ByteString (ByteString)
import qualified Database.Redis as Redis
import Data.Serialize (Serialize, encode, decode)

connect' :: IO Redis.Connection
connect' = Redis.connect Redis.defaultConnectInfo { Redis.connectHost = "172.17.0.8" }

class (Functor m, Applicative m, Monad m) => MonadCache m where
    get :: (Serialize a) => ByteString -> m (Maybe a)
    set :: (Serialize a) => ByteString -> a -> m ()

instance MonadCache IO where
    get key = do
        conn <- connect'
        val <- Redis.runRedis conn $ do
            v <- Redis.get key
            case v of
                Right v' -> return v'
                Left  r  -> fail (show r)
        return $ do
            v <- val
            case decode v of
                Right v' -> return v'
                Left err -> fail err
    set key val = do
        conn <- connect'
        Redis.runRedis conn $ do
            res <- Redis.set key (encode val)
            case res of
                Right _ -> return ()
                Left  r -> fail (show r)
