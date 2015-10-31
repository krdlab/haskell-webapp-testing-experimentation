{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import WS.App (api, server)

main :: IO ()
main = run 8081 app

app :: Application
app = serve api server
