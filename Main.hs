{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs
import System.Exit
import System.FilePath
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
    &= help "Map requests directly to static file directories"
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
  logger <- stdoutApacheLoggerInit FromSocket True
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  W.runSettings warpSettings $ app logger opts


app :: ApacheLogger -> Nixrbd -> Application
app logger opts req = case lookup root (nixrbdMap opts) of
  Nothing -> do
    buildRes <- liftIO $ nixBuild (nixArgs opts req) (nixrbdDefaultExpr opts)
    either respondFailed serveFile buildRes
  Just dir -> do
    dir' <- liftIO $ Dir.canonicalizePath dir
    fp <- liftIO $ Dir.canonicalizePath $ combine dir' $ joinPath $ tail ps
    exists <- liftIO $ Dir.doesFileExist fp
    if exists && isPrefixOf dir' fp
      then serveFile fp
      else respondNotFound fp
  where
    root = if null ps then "" else head ps
    ps = map T.unpack (pathInfo req)
    stringResp s = return . responseLBS s [("Content-Type","text/plain")] . pack
    respondFailed err = do
      liftIO $ logger req internalServerError500 Nothing
      stringResp internalServerError500 ("Failed building response: "++show err)
    respondNotFound fp = do
      liftIO $ logger req notFound404 Nothing
      stringResp notFound404 ("Not found: "++fp)
    serveFile filePath = do
      liftIO $ logger req status200 Nothing
      return $ ResponseFile status200 [] filePath Nothing


nixBuild :: [String] -> String -> IO (Either String String)
nixBuild as file = do
  (r1,o1,e1) <- readProcessWithExitCode "nix-instantiate" (file:as) ""
  let [o1',e1'] = map (T.unpack . T.strip . T.pack) [o1,e1]
  if r1 /= ExitSuccess
    then return $ Left $ "nix-instantiate failed: "++e1'
    else do
      (r2,o2,e2) <- readProcessWithExitCode "nix-store" ["-r",o1'] ""
      let [o2',e2'] = map (T.unpack . T.strip . T.pack) [o2,e2]
      if r2 /= ExitSuccess
        then return $ Left $ "nix-store failed: "++e2'
        else do
          exists <- Dir.doesFileExist o2'
          return $ if not exists
            then Left $ "nix-store output not a file: "++o2'
            else Right o2'


nixArgs :: Nixrbd -> Request -> [String]
nixArgs opts req =
  "--arg" : "request" : reqToNix req :
  "-Q" : concat [["-I",p] | p <- nixrbdNixPath opts]

listToNix :: Show a => [a] -> String
listToNix xs = "[" ++ unwords (map show xs) ++ "]"

reqToNix :: Request -> String
reqToNix req = concat 
  [ "{"
  , "pathInfo = ", listToNix (pathInfo req), ";"
  , "}"
  ]
