{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy.Char8 ()
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs

data Nixrbd = Nixrbd {
  nixrbdPort :: Int,
  nixrbdConfigFile :: FilePath
} deriving (Show, Data, Typeable)

nixrbdDefs = Nixrbd
  { nixrbdPort = 8000
    &= explicit
    &= name "p" &= name "port"
    &= help "TCP port to bind to"
  , nixrbdConfigFile = ""
    &= explicit
    &= typFile
    &= name "c" &= name "configfile"
    &= help "Path to configuration file"
  } &= summary "Nix Remote Boot Daemon v0.0"

warpSettings opts = W.defaultSettings {
  W.settingsPort = nixrbdPort opts
}

main = do
  opts @ Nixrbd { nixrbdConfigFile = cf } <- cmdArgs nixrbdDefs
  putStrLn (show $ nixrbdPort opts)
  W.runSettings (warpSettings opts) app

app :: Application
app req = return $ responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Hello, World"
