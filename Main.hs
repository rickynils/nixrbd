{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy.Char8 ()
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs

data Nixrbd = Nixrbd {
  port :: Int
} deriving (Show, Data, Typeable)

nixrbdOpts = Nixrbd {
  port = 8000
}

warpSettings :: Nixrbd -> W.Settings
warpSettings opts = W.defaultSettings {
  W.settingsPort = port opts
}

main = do
  opts @ Nixrbd { } <- cmdArgs nixrbdOpts
  W.runSettings (warpSettings opts) app

app :: Application
app req = return $ responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Hello, World"
