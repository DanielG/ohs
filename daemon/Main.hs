{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Either
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Format
import Data.Traversable
import Data.Word
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Network.HTTP.Client.Internal

import System.Locale

import OHS.Types
import OHS.Server

webKit_userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/538.15 (KHTML, like Gecko) Version/8.0 Safari/538.15"

main = server "127.0.0.1" "1234"

-- | UTF8 or JavaScript escaped cookie string for use with @document.cookie@
cookieToJSDOM :: Cookie -> String
cookieToJSDOM Cookie {..} = fromJust $
    domify decodeUtf8 <|> Just (runIdentity $ domify decodeStupid)

 where
   domify :: Monad m => (BS.ByteString -> m String) -> m String
   domify decode = do
     key     <- decode cookie_name
     value   <- decode cookie_value
     domain  <- decode cookie_domain
     path    <- decode cookie_path

     let expires = utcString cookie_expiry_time
         secure  = if cookie_secure_only then "true" else "false"


-- TODO TODO: only return cookies that are relevant to whatever request, should
-- probably be done in the UI. For google APISID is the only cookie needed for
-- www.google.com stuff to work. All the other shit seems to be for passing
-- around the authentication info
     return $ concat [key, "=",  value
--                   "; Domain=",  domain
--                  "; expires=",  expires,
--                     "; path=",  path,
--                   "; secure=",  secure
                     ]

   utcString = formatTime defaultTimeLocale rfc822DateFormat
   encodeURIComponent = id -- escapeURIString isUnescapedInURIComponent -- (\c -> isUnescapedInURIComponent c || c `elem` "=;")

   decodeUtf8 :: BS.ByteString -> Maybe String
   decodeUtf8 bs = encodeURIComponent
               <$> T.unpack <$> either (const Nothing) Just (decodeUtf8' bs)

   decodeStupid :: Monad m => BS.ByteString -> m String
   decodeStupid bs = return $ concat $ stupid <$> BS.unpack bs

   stupid :: Word8 -> String
   stupid w = let c = chr (fromIntegral w)
              in
                if isPrint c
                   then encodeURIComponent [c]
                   else "\\x" ++ showHex w ""

  -- { cookie_name :: S.ByteString
  -- , cookie_value :: S.ByteString
  -- , cookie_expiry_time :: UTCTime
  -- , cookie_domain :: S.ByteString
  -- , cookie_path :: S.ByteString
  -- , cookie_creation_time :: UTCTime
  -- , cookie_last_access_time :: UTCTime
  -- , cookie_persistent :: Bool
  -- , cookie_host_only :: Bool
  -- , cookie_secure_only :: Bool
  -- , cookie_http_only :: Bool
  -- }
  -- deriving (Read, Show, T.Typeable)


{-
GALX:2-vz4I3OHp0
_utf8:☃
bgresponse:js_disabled
pstMsg:0
dnConn:
checkConnection:
checkedDomains:youtube
Email:nobody@example.com
Passwd:example
signIn:Sign in
PersistentCookie:yes
rmShown:1
-}
