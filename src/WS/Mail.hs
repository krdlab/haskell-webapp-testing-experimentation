{-# LANGUAGE OverloadedStrings #-}
module WS.Mail where

import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Network.Mail.SMTP as SMTP

class Monad m => MonadMail m where
    sendMail :: Text -> Text -> Text -> Text -> m ()

instance MonadMail IO where
    sendMail from to sub body = sendMail' "localhost" 1025 mail
      where
        mail  = simpleMail from' [to'] [] [] sub [plainTextPart (fromStrict body)]
        from' = Address Nothing from
        to'   = Address Nothing to
