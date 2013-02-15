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
import System.Log.Logger
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
    &= help "A Nix file that should handle all requests"
  } &= summary "Nix Remote Boot Daemon v0.0"


main :: IO ()
main = do
  aplogger <- stdoutApacheLoggerInit FromSocket True
  updateGlobalLogger "nixrbd" (setLevel DEBUG)
  infoM "nixrbd" "Initialising daemon"
  opts <- cmdArgs nixrbdDefs
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  infoM "nixrbd" ("Listening on port "++show (nixrbdPort opts))
  W.runSettings warpSettings $ app aplogger opts


app :: ApacheLogger -> Nixrbd -> Application
app aplogger opts req = case lookup root (nixrbdMap opts) of
  Nothing -> do
    buildRes <- liftIO $ nixBuild (nixArgs opts req) (nixrbdDefaultExpr opts)
    either respondFailed serveFile buildRes
  Just dir -> do
    let fp = combine dir $ joinPath $ tail ps
    exists <- liftIO $ Dir.doesFileExist fp
    if not exists then respondNotFound fp else do
      dir' <- liftIO $ Dir.canonicalizePath dir
      fp' <- liftIO $ Dir.canonicalizePath fp
      if isPrefixOf dir' fp'
        then serveFile fp'
        else respondNotFound fp'
  where
    root = if null ps then "" else head ps
    ps = map T.unpack (pathInfo req)
    stringResp s = return . responseLBS s [("Content-Type","text/plain")] . pack
    respondFailed err = do
      liftIO $ errorM "nixrbd" ("Failure: "++show err)
      liftIO $ aplogger req internalServerError500 Nothing
      stringResp internalServerError500 ("Failed building response")
    respondNotFound fp = do
      liftIO $ infoM "nixrbd" ("Not found: "++fp)
      liftIO $ aplogger req notFound404 Nothing
      stringResp notFound404 ("Not found: "++fp)
    serveFile filePath = do
      liftIO $ infoM "nixrbd" ("Serve file: "++filePath)
      liftIO $ aplogger req status200 Nothing
      return $ ResponseFile status200 [] filePath Nothing


nixBuild :: [String] -> String -> IO (Either String String)
nixBuild as file = do
  infoM "nixrbd" ("Executing nix-instantiate " ++ unwords (file:as))
  (r1,o1,e1) <- readProcessWithExitCode "nix-instantiate" (file:as) ""
  let [o1',e1'] = map (T.unpack . T.strip . T.pack) [o1,e1]
  if r1 /= ExitSuccess
    then return $ Left $ "nix-instantiate failed: "++e1'
    else do
      infoM "nixrbd" ("Executing nix-build "++o1')
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
  "--show-trace" :
  "-Q" : concat [["-I",p] | p <- nixrbdNixPath opts]

listToNix :: Show a => [a] -> String
listToNix xs = "[" ++ unwords (map show xs) ++ "]"

reqToNix :: Request -> String
reqToNix req = concat 
  [ "{"
  , "pathInfo = ", listToNix (pathInfo req), ";"
  , "}"
  ]
