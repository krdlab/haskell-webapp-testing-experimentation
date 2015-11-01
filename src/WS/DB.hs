{-# LANGUAGE FlexibleContexts #-}
module WS.DB where

import Control.Applicative (Applicative, (<*))
import Database.HDBC (SqlValue, commit)
import Database.HDBC.MySQL (Connection, connectMySQL, MySQLConnectInfo(..), defaultMySQLConnectInfo)
import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.Record (runQuery', runInsert, runUpdate)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.HDBC.Session (withConnectionIO)
import Database.Record (FromSql, ToSql)
import Database.Relational.Query (Config (..), defaultConfig, Update, Insert, Query)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

connect :: IO Connection
connect = connectMySQL info
  where
    info = defaultMySQLConnectInfo {
              mysqlUser = "tester"
            , mysqlPassword = "tester"
            , mysqlDatabase = "main"
            , mysqlHost = "172.17.0.7"
            }

config :: Config
config =  defaultConfig { normalizedTableName = False }

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap = defineTableFromDB' connect config (driverMySQL { typeMap = tmap })

class (Functor m, Applicative m, Monad m) => MonadDB m where
    select :: (ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> m [a]
    insert :: (ToSql SqlValue p) => Insert p -> p -> m Integer
    update :: (ToSql SqlValue p) => Update p -> p -> m Integer

instance MonadDB IO where
    select q ps = withConnectionIO connect $ \conn -> runQuery' conn q ps
    insert i ps = withConnectionIO connect $ \conn -> runInsert conn i ps <* commit conn
    update u ps = withConnectionIO connect $ \conn -> runUpdate conn u ps <* commit conn
