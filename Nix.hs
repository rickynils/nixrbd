module Nix
  ( nixBuild
  ) where

import Route
import Conf

import Control.Monad
import Network.Wai
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process (readProcessWithExitCode)
import System.Directory
import qualified Data.Text as T

nixBuild :: Nixrbd -> Request -> String -> Path -> IO (Either String String)
nixBuild conf req file path = do
  let buildArgs = nixBuildArgs conf file path req
  infoM "nixrbd" ("Executing nix-instantiate " ++ unwords buildArgs)
  (r1,o1,e1) <- readProcessWithExitCode "nix-instantiate" buildArgs ""
  let [o1',e1'] = map (T.unpack . T.strip . T.pack) [o1,e1]
  if r1 /= ExitSuccess
    then return $ Left $ "nix-instantiate failed: "++e1'
    else do
      infoM "nixrbd" ("Derivation: "++o1')
      prevBuild <- lookupPreviousBuild conf o1'
      case prevBuild of
        Just p -> do
          infoM "nixrbd" ("Existing build found: "++p)
          return (Right p)
        Nothing -> do 
          storeArgs <- nixStoreArgs conf o1'          
          infoM "nixrbd" ("Executing nix-store " ++ unwords storeArgs)
          (r2,o2,e2) <- readProcessWithExitCode "nix-store" storeArgs ""
          let [o2',e2'] = map (T.unpack . T.strip . T.pack) [o2,e2]
          if r2 /= ExitSuccess
            then return $ Left $ "nix-store failed: "++e2'
            else do
              exists <- doesFileExist o2'
              return $ if not exists
                then Left $ "nix-store output not a file: "++o2'
                else Right o2'


-- Private interface --

listToNix :: Show a => [a] -> String
listToNix xs = "[" ++ unwords (map show xs) ++ "]"

reqToNix :: Path -> Request -> String
reqToNix path req = concat
  [ "{"
  , "fullPath = ", listToNix (pathInfo req), ";"
  , "path = ", listToNix path, ";"
  , "}"
  ]

nixBuildArgs :: Nixrbd -> FilePath -> Path -> Request -> [String]
nixBuildArgs opts file path req =
  file : "--arg" : "request" : reqToNix path req : "--show-trace" :
  "-Q" : concat [["-I",p] | p <- nixrbdNixPath opts]

nixStoreArgs :: Nixrbd -> FilePath -> IO [String]
nixStoreArgs conf drvPath = do
  buildDirExists <- doesDirectoryExist (nixrbdBuildDir conf)
  unless buildDirExists $ infoM "nixrbd" "No build dir. Not saving results" 
  let linkArgs = ["--indirect", "--add-root", buildLinkName conf drvPath]
  return $ "--realise" : drvPath : if buildDirExists then linkArgs else []

buildLinkName :: Nixrbd -> FilePath -> FilePath
buildLinkName conf drvPath = 
  combine (nixrbdBuildDir conf) (takeFileName drvPath)

lookupPreviousBuild :: Nixrbd -> String -> IO (Maybe FilePath)
lookupPreviousBuild conf drvPath = do
  let linkName = buildLinkName conf drvPath
  exists <- doesFileExist linkName
  return $ if not exists then Nothing else Just linkName
