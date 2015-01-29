{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module OHS.Cookies where

import Control.Applicative
import Data.List
import Data.Char (ord)
import Network.HTTP.Client
import Web.Cookie

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

evictOverwrittenCookies :: [Cookie] -> [Cookie]
evictOverwrittenCookies cs = cs
    -- map last $
    -- map (sortBy (\a b -> cookie_creation_time a `compare` cookie_creation_time b)) $
    --  groupBy (\a b -> cookie_name a == cookie_name b) $
    --  sortBy  (\a b -> cookie_name a `compare` cookie_name b) cs

cookieJarToAsc :: CookieJar -> [(String, BS.ByteString)]
cookieJarToAsc jar = cookieToAsc <$> destroyCookieJar jar

cookieToAsc :: Cookie -> (String,BS.ByteString)
cookieToAsc Cookie {..} = (C8.unpack cookie_name, cookie_value)

cookieToSetCookie :: Cookie -> (Maybe ByteString, SetCookie)
cookieToSetCookie Cookie {..} = let
    (dom, cdom) = if cookie_host_only
                    then (Just cookie_domain, Nothing)
                    else (Nothing, Just cookie_domain)

-- )case BS.unpack cookie_domain of
--           (c:_) | c == fromIntegral (ord '.') -> (Just cookie_domain, Nothing)
--           _ | BS.null cookie_domain -> (Nothing, Nothing)
--             | otherwise -> (Nothing, Just $ C8.unpack cookie_domain)
   in (,) dom def {
                 setCookieName = cookie_name
               , setCookieValue = cookie_value
               , setCookiePath = Just cookie_path
               , setCookieExpires = Just cookie_expiry_time
               , setCookieMaxAge = Nothing
               , setCookieDomain = cdom
               , setCookieHttpOnly = cookie_http_only
               , setCookieSecure = cookie_secure_only
               }
