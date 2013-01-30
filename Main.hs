{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs
import System.Exit
import System.Process (readProcessWithExitCode)
import qualified System.Directory as Dir

data Nixrbd = Nixrbd
  { nixrbdPort :: Int
  , nixrbdConfigFile :: FilePath
  , nixrbdMap :: [(String,String)]
  , nixrbdNixPath :: [String]
  , nixrbdDefaultExpr :: FilePath
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
  , nixrbdDefaultExpr = ""
    &= explicit
    &= name "d" &= name "default"
    &= help "Map all un-mapped requests to this Nix file"
  } &= summary "Nix Remote Boot Daemon v0.0"


main :: IO ()
main = do
  opts <- cmdArgs nixrbdDefs
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  W.runSettings warpSettings $ app opts


app :: Nixrbd -> Application
app opts req = case (lookup p $ nixrbdMap opts, nixrbdDefaultExpr opts) of
  (Nothing, "") -> respondNotFound
  (Nothing, f) -> build f
  (Just f, _) -> build f
  where
    build f = liftIO (nixBuild opts f) >>= either respondFailed serveFile
    p = head $ map T.unpack (pathInfo req) ++ ["/"]
    stringResp s = return . responseLBS s [("Content-Type","text/plain")] . pack
    respondFailed err = do
      liftIO $ putStrLn $ "[500] Build failed for "++p++": "++err
      stringResp internalServerError500 ("Failed building: "++p)
    respondNotFound = do
      liftIO $ putStrLn $ "[404] No map for "++p
      stringResp notFound404 ("Not found: "++p)
    serveFile filePath = do
      liftIO $ putStrLn $ "[200] "++p++" => "++filePath
      return $ ResponseFile status200 [] filePath Nothing


nixBuild :: Nixrbd -> String -> IO (Either String String)
nixBuild opts nixFile = do
  let nixArgs = "-Q" : map ("-I "++) (nixrbdNixPath opts)
  (r1,o1,e1) <- readProcessWithExitCode "nix-instantiate" (nixFile:nixArgs) ""
  let [o1',e1'] = map (T.unpack . T.strip . T.pack) [o1,e1]
  if (r1 /= ExitSuccess)
    then return $ Left $ "nix-instantiate failed: "++e1'
    else do
      (r2,o2,e2) <- readProcessWithExitCode "nix-store" ["-r",o1'] ""
      let [o2',e2'] = map (T.unpack . T.strip . T.pack) [o2,e2]
      if (r2 /= ExitSuccess)
        then return $ Left $ "nix-store failed: "++e2'
        else do
          exists <- Dir.doesFileExist o2'
          return $ if not exists
            then Left $ "nix-store output not a file: "++o2'
            else Right o2'
