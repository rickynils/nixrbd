{-# LANGUAGE OverloadedStrings #-}

module Main where

import Route
import Nix
import Conf

import Control.Monad.Error ()
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Default (def)
import Data.List (isPrefixOf)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import qualified Network.Wai.Handler.Warp as W
import System.FilePath
import System.Log.Logger
import qualified System.Directory as Dir

main :: IO ()
main = do
  opts <- parseOpts
  routes <- either fail return (mapM parseRoute (nixrbdRoutes opts))
  let addrSource = if nixrbdBehindProxy opts then FromHeader else FromSocket
  reqLogger <- mkRequestLogger $ def { outputFormat = Apache addrSource }
  updateGlobalLogger "nixrbd" (setLevel DEBUG)
  let warpSettings = W.defaultSettings { W.settingsPort = nixrbdPort opts }
  infoM "nixrbd" ("Listening on port "++show (nixrbdPort opts))
  W.runSettings warpSettings $ reqLogger $ app routes opts


app :: [Route] -> Nixrbd -> Application
app routes opts req = case lookupTarget req routes of
  (NixHandler p, ps') -> do
    buildRes <- liftIO $ nixBuild opts req p ps'
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
      filePath' <- liftIO $ Dir.canonicalizePath filePath
      liftIO $ infoM "nixrbd" ("Serve file: "++filePath')
      return $ ResponseFile status200 [] filePath' Nothing
