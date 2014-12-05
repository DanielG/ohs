{-# LANGUAGE RecordWildCards #-}
module OHS.Cookies where

import Control.Applicative
import Data.List
import Network.HTTP.Client

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

evictOverwrittenCookies :: [Cookie] -> [Cookie]
evictOverwrittenCookies cs =
    map last $
    map (sortBy (\a b -> cookie_creation_time a `compare` cookie_creation_time b)) $
     groupBy (\a b -> cookie_name a == cookie_name b) $
     sortBy  (\a b -> cookie_name a `compare` cookie_name b) cs

cookieJarToAsc :: CookieJar -> [(String, BS.ByteString)]
cookieJarToAsc jar = cookieToAsc <$> destroyCookieJar jar

cookieToAsc :: Cookie -> (String,BS.ByteString)
cookieToAsc Cookie {..} = (C8.unpack cookie_name, cookie_value)
