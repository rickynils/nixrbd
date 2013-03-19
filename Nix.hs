module Nix
  ( nixBuild
  ) where

import Route
import Conf
import Network.Wai
import System.Exit
import System.Log.Logger
import System.Process (readProcessWithExitCode)
import qualified System.Directory as Dir
import qualified Data.Text as T

nixBuild :: Nixrbd -> Request -> String -> Path -> IO (Either String String)
nixBuild conf req file path = do
  let as = nixArgs conf path req
  infoM "nixrbd" ("Executing nix-instantiate " ++ unwords (file:as))
  (r1,o1,e1) <- readProcessWithExitCode "nix-instantiate" (file:as) ""
  let [o1',e1'] = map (T.unpack . T.strip . T.pack) [o1,e1]
  if r1 /= ExitSuccess
    then return $ Left $ "nix-instantiate failed: "++e1'
    else do
      infoM "nixrbd" ("Executing nix-store --realise "++o1')
      (r2,o2,e2) <- readProcessWithExitCode "nix-store" ["-r",o1'] ""
      let [o2',e2'] = map (T.unpack . T.strip . T.pack) [o2,e2]
      if r2 /= ExitSuccess
        then return $ Left $ "nix-store failed: "++e2'
        else do
          exists <- Dir.doesFileExist o2'
          return $ if not exists
            then Left $ "nix-store output not a file: "++o2'
            else Right o2'


-- Private interface --

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
