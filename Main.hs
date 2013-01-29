{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (unpack)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs

data Nixrbd = Nixrbd
  { nixrbdPort :: Int
  , nixrbdConfigFile :: FilePath
  , nixrbdMap :: [(String,String)]
  , nixrbdNixPath :: [String]
  } deriving (Show, Data, Typeable)

nixrbdDefs :: Nixrbd
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
  , nixrbdMap = []
    &= explicit
    &= name "m" &= name "map"
    &= help "Map a request to a Nix file"
  , nixrbdNixPath = []
    &= explicit
    &= name "I"
    &= help "Add a path used by nix-build"
  } &= summary "Nix Remote Boot Daemon v0.0"


main :: IO ()
main = do
  opts <- cmdArgs nixrbdDefs
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  W.runSettings warpSettings $ app opts


app :: Nixrbd -> Application
app opts req = case lookup p $ nixrbdMap opts of
  Nothing -> respondNotFound
  Just f -> liftIO (nixBuild opts f) >>= either respondFailed serveFile
  where
    p = head $ map unpack (pathInfo req) ++ ["/"]
    stringResp s = return . responseLBS s [("Content-Type","text/plain")] . pack
    respondFailed err = do
      liftIO $ putStrLn $ "[500] Build failed for "++p++": "++show err
      stringResp internalServerError500 ("Failed building: "++p)
    respondNotFound = do
      liftIO $ putStrLn $ "[404] No map for "++p
      stringResp notFound404 ("Not found: "++p)
    serveFile filePath = do
      liftIO $ putStrLn $ "[200] "++p++" => "++filePath
      return $ ResponseFile status200 [] filePath Nothing


-- Work in progress
nixBuild :: Nixrbd -> String -> IO (Either String String)
nixBuild _ nixFile = case nixFile of
  "err" -> return $ Left "Error!"
  _ -> return $ Right nixFile
