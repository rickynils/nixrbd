{-# LANGUAGE DeriveDataTypeable #-}

module Conf
  ( Nixrbd(..)
  , parseOpts
  ) where

import System.Console.CmdArgs hiding (def)

data Nixrbd = Nixrbd
  { nixrbdPort :: Int
  , nixrbdConfigFile :: FilePath
  , nixrbdNixPath :: [String]
  , nixrbdBehindProxy :: Bool
  , nixrbdRoutes :: [(String,String)]
  , nixrbdBuildDir :: FilePath
  } deriving (Show, Data, Typeable)

parseOpts :: IO Nixrbd
parseOpts = cmdArgs nixrbdDefs

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
  , nixrbdBuildDir = ""
    &= explicit
    &= name "d" &= name "builddir"
    &= help "Directory to store build result symlinks in"
  } &= summary "Nix Remote Boot Daemon v0.1.1"
