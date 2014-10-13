{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Data.Maybe
import Data.Traversable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData

import Network.HTTP.Client.Internal


main = withManager tlsManagerSettings $ \mngr -> do
  initReq <- parseUrl "https://accounts.google.com"
  let req = initReq

  print req

  withResponse' req mngr $ \(rs, (req',res')) -> do
      _ <- sequenceA $ brConsume <$> res'

      print $ (path req') `BS.append` (queryString req')

      loginReq <- googleLoginReq (responseCookieJar res')

      putStrLn "" >> print loginReq >> putStrLn ""

      withResponse' loginReq mngr $ \(_, (req'', res'')) -> do
          res'' <- sequenceA $ brConsume <$> res''

          putStrLn "" >> putStrLn ""

          print $ (path req'') `BS.append` (queryString req'')

          putStrLn ""

          print $ cookieJarToAsc $ responseCookieJar res''

 where
   withResponse' req mngr f = do
       bracket (responseOpen' req mngr) (responseClose . snd . snd) f

googleLoginReq :: CookieJar -> IO Request
googleLoginReq jar = do
    url <- parseUrl "https://accounts.google.com/ServiceLoginAuth"

    formDataBody [ partBS "GALX" galx
                 , partBS "_utf8" "☃"
                 , partBS "bgresponse" "js_disabled"
                 , partBS "Email" "smith@darkboxed.org"
                 , partBS "Passwd" "iN1qohQK45ATr3ygpYxC"
                 , partBS "signIn" "Sign in"
                 , partBS "PersistentCookie" "yes"
                 , partBS "rmShown" "1"
                 ] url { cookieJar = Just jar }

 where
   galx = fromJust $ lookup "GALX" (cookieJarToAsc jar)


cookieJarToAsc :: CookieJar -> [(String, BS.ByteString)]
cookieJarToAsc jar = cookieToAsc <$> destroyCookieJar jar

cookieToAsc :: Cookie -> (String,BS.ByteString)
cookieToAsc Cookie {..} = (C8.unpack cookie_name, cookie_value)

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
