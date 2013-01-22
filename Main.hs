{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

data Nixrbd = Nixrbd {
} deriving (Show, Data, Typeable)

nixrbdOpts = Nixrbd {
}

main = do
  opts @ Nixrbd { } <- cmdArgs nixrbdOpts
  return ()
