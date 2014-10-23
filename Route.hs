{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TupleSections #-}
module Route
  ( Route
  , Path
  , Target(..)
  , parseRoute
  , lookupTarget
  ) where

import Data.ByteString.Char8 (unpack)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Network.HTTP.Types
import Network.URI
import Network.Wai

type NixPathElem = String

data Target = NixHandler FilePath [NixPathElem]
            | StaticPath FilePath
            | StaticResp Status
            deriving (Show, Eq)

type Route = (URI,Target)

type Path = [String]

parseRoute :: (String,String) -> Either String Route
parseRoute (p,t) = do
  uPath <- maybe (fail $ "Not a path: "++t) return (parseRelativeReference p)
  uTarget <- maybe (fail $ "Not an URI: "++t) return (parseURI t)
  t' <- parseTarget uTarget
  return (uPath,t')

lookupTarget :: Request -> [Route] -> (Target,Path)
lookupTarget req routes = fromMaybe (StaticResp notFound404, reqPath req) $ do
  let routes' = filter (reqMatchUri req . fst) routes
  (u,t) <- listToMaybe $ sortBy (orderUri `on` fst) routes'
  return (t, drop (length $ uriPath' u) (reqPath req))


-- Private interface --

parseTarget :: URI -> Either String Target
parseTarget uri = case uriScheme uri of
  "nix:" -> return $ NixHandler (uriPath uri) []
  "file:" -> return $ StaticPath (uriPath uri)
  "resp:" -> case reads (dropWhile (not . isDigit) (uriPath uri)) of
    [(n,"")] -> return $ StaticResp (mkStatus n "")
    _ -> fail $ "Not a valid response status: " ++ uriPath uri
  s -> fail $ "Not a valid URI scheme: " ++ s

reqMatchUri :: Request -> URI -> Bool
reqMatchUri req uri =
  (uriPath' uri `isPrefixOf` reqPath req) &&
  all (flip elem $ reqQuery req) (uriQuery' uri)

orderUri :: URI -> URI -> Ordering
orderUri u1 u2 = case comparing (length . uriPath') u1 u2 of
  LT -> GT
  GT -> LT
  EQ -> comparing (length . uriQuery') u2 u1

reqPath :: Request -> Path
reqPath req = map T.unpack (pathInfo req)

reqQuery :: Request -> [String]
reqQuery req = map qiStr (queryString req)
  where
    qiStr (p, Nothing) = unpack p
    qiStr (p, Just v) = unpack p ++ "=" ++ unpack v

uriPath' :: URI -> Path
uriPath' uri = split ((dropDelims . dropBlanks) (onSublist "/")) (uriPath uri)

uriQuery' :: URI -> [String]
uriQuery' uri =
  split ((dropDelims . dropBlanks) (onSublist "&")) (drop 1 $ uriQuery uri)
