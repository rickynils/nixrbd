{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TupleSections #-}
module Main where

import Control.Monad.Error ()
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (isDigit)
import Data.Default (def)
import Data.List (isPrefixOf, find, sortBy)
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Network.HTTP.Types
import Network.URI (URI, parseURI, uriScheme, uriPath)
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import qualified Network.Wai.Handler.Warp as W
import System.Console.CmdArgs hiding (def)
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process (readProcessWithExitCode)
import qualified System.Directory as Dir

data Nixrbd = Nixrbd
  { nixrbdPort :: Int
  , nixrbdConfigFile :: FilePath
  , nixrbdNixPath :: [String]
  , nixrbdBehindProxy :: Bool
  , nixrbdRoutes :: [(String,String)]
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
  , nixrbdNixPath = []
    &= explicit
    &= name "I"
    &= help "Add a path used by nix-build"
  , nixrbdBehindProxy = False
    &= explicit
    &= name "b" &= name "proxied"
    &= help "Wether nixrbd is running behind a proxy or not"
  , nixrbdRoutes = []
    &= explicit
    &= name "r" &= name "route"
    &= help "Add a route"
  } &= summary "Nix Remote Boot Daemon v0.1.1"


data Target = NixHandler FilePath
            | StaticPath FilePath
            | StaticResp Status
            deriving (Show, Eq)

type Path = [String]

type Route = (Path,Target)

parseRoute :: (String,String) -> Either String Route
parseRoute (p,t) = do
  let p' = split ((dropDelims . dropBlanks) (onSublist "/")) p
  uri <- maybe (fail $ "Not an URI: "++t) return (parseURI t)
  t' <- parseTarget uri
  return (p',t')

parseTarget :: URI -> Either String Target
parseTarget uri = case uriScheme uri of
  "nix:" -> return $ NixHandler (uriPath uri)
  "file:" -> return $ StaticPath (uriPath uri)
  "resp:" -> case reads (dropWhile (not . isDigit) (uriPath uri)) of
    [(n,"")] -> return $ StaticResp (mkStatus n "")
    _ -> fail $ "Not a valid response status: " ++ uriPath uri
  s -> fail $ "Not a valid URI scheme: " ++ s

lookupTarget :: Path -> [Route] -> (Target,Path)
lookupTarget path routes = fromMaybe (StaticResp notFound404, path) $ do
  let ordRoutes = sortBy (comparing ((0-) . length . fst)) routes
  (prefix,t) <- find ((`isPrefixOf` path) . fst) ordRoutes
  return (t, drop (length prefix) path)


main :: IO ()
main = do
  opts <- cmdArgs nixrbdDefs
  routes <- either fail return (mapM parseRoute (nixrbdRoutes opts))
  let addrSource = if nixrbdBehindProxy opts then FromHeader else FromSocket
  reqLogger <- mkRequestLogger $ def { outputFormat = Apache addrSource }
  updateGlobalLogger "nixrbd" (setLevel DEBUG)
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  infoM "nixrbd" ("Listening on port "++show (nixrbdPort opts))
  W.runSettings warpSettings $ reqLogger $ app routes opts


app :: [Route] -> Nixrbd -> Application
app routes opts req = case lookupTarget (map T.unpack (pathInfo req)) routes of
  (NixHandler p, ps') -> do
    buildRes <- liftIO $ nixBuild (nixArgs opts ps' req) p
    either respondFailed serveFile buildRes
  (StaticPath p, ps') -> do
    let fp = combine p (joinPath ps')
    exists <- liftIO $ Dir.doesFileExist fp
    if not exists then respondNotFound fp else do
      p' <- liftIO $ Dir.canonicalizePath p
      fp' <- liftIO $ Dir.canonicalizePath fp
      if p' `isPrefixOf` fp'
        then serveFile fp'
        else respondNotFound fp'
  (StaticResp s, _) -> stringResp s ""
  where
    stringResp s = return . responseLBS s [("Content-Type","text/plain")] . pack
    respondFailed err = do
      liftIO $ errorM "nixrbd" ("Failure: "++show err)
      stringResp internalServerError500 "Failed building response"
    respondNotFound fp = do
      liftIO $ infoM "nixrbd" ("Not found: "++fp)
      stringResp notFound404 "Not found"
    serveFile filePath = do
      liftIO $ infoM "nixrbd" ("Serve file: "++filePath)
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


nixArgs :: Nixrbd -> Path -> Request -> [String]
nixArgs opts path req =
  "--arg" : "request" : reqToNix path req :
  "--show-trace" :
  "-Q" : concat [["-I",p] | p <- nixrbdNixPath opts]

listToNix :: Show a => [a] -> String
listToNix xs = "[" ++ unwords (map show xs) ++ "]"

reqToNix :: Path -> Request -> String
reqToNix path req = concat
  [ "{"
  , "fullPath = ", listToNix (pathInfo req), ";"
  , "path = ", listToNix path, ";"
  , "}"
  ]
