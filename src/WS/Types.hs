{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WS.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import Data.Fixed (Pico)
import Data.Serialize
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (LocalTime(..), Day(..), TimeOfDay(..))
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Record.TH (derivingShow)
import Database.Relational.Query
import GHC.Generics (Generic)
import Language.Haskell.TH.Name.CamelCase (conCamelcaseName)
import Prelude hiding (id)
import WS.DB

--data User = User
--    { id             :: Int
--    , name           :: Text
--    , emailAddress   :: Text
--    , createdAt      :: LocalTime
--    , lastLoggedinAt :: LocalTime
--    }
--  deriving (Eq, Generic, Show)

$(defineTable
    [("VARCHAR", [t|Text|])]
    "test"
    "user"
    [derivingShow, conCamelcaseName "Generic"])

data InsertUser = InsertUser
    { insName           :: Text
    , insEmailAddress   :: Text
    , insCreatedAt      :: LocalTime
    , insLastLoggedinAt :: LocalTime
    }
$(makeRecordPersistableDefault ''InsertUser)

piUser :: Pi User InsertUser
piUser = InsertUser |$| name'
                    |*| emailAddress'
                    |*| createdAt'
                    |*| lastLoggedinAt'

instance Serialize User

instance Serialize Text where
    put = put . encodeUtf8
    get = decodeUtf8 <$> get

instance Serialize Day where
    put = put . toModifiedJulianDay
    get = ModifiedJulianDay <$> get

instance Serialize TimeOfDay where
    put tod = do
        put $ todHour tod
        put $ todMin  tod
        put $ todSec  tod
    get = TimeOfDay <$> get <*> get <*> get

instance Serialize Pico where
    put = put . show
    get = read <$> get

instance Serialize LocalTime where
    put lt = do
        put $ localDay lt
        put $ localTimeOfDay lt
    get = LocalTime <$> get <*> get

instance ToJSON User where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data RegForm = RegForm
    { regName         :: Text
    , regEmailAddress :: Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON RegForm where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data LoginForm = LoginForm
    { loginName :: Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON LoginForm where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
