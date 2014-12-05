{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module OHS.Server where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Serialize
import Data.Serialize.Get
import Data.Traversable
import Network.Simple.TCP
import Network.Socket.ByteString.Lazy as NBL
import Network.Socket.ByteString as NB
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Header
import Pipes as P
import Pipes.Network.TCP as PT

import qualified Network.Protocol.SASL.GNU as SASL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding
import qualified Data.Text as T

import Data.Maybe

import OHS.Types
import OHS.Cookies

decoderPipe :: MonadIO m => Get a -> Pipe BS.ByteString a m ()
decoderPipe get = go (runGetPartial get) BS.empty
 where
   go f bs =
       case f bs of
         Fail err rest -> fail err
         Partial f' -> await >>= go f'
         Done r rest -> yield r >> go (runGetPartial get) rest

encodePipe :: Monad m => Putter a -> Pipe a BS.ByteString m ()
encodePipe put =
  yield =<< (runPut . put) <$> await

server :: String -> String -> IO ()
server host port = serve (Host host) port $ \(s,addr) -> cleanup s $ do
        let
          p = PT.fromSocket s 4096
                >-> decoderPipe (get :: Get Command)
                >-> (await >>= liftIO . executeCommand >>= yield)
                >-> encodePipe (put :: Putter CommandResponse)
                >-> toSocket s
        runEffect p
 where
  cleanup s = flip finally (closeSock s)

loginMethods :: [(Site, LoginMethod)]
loginMethods = [
 (,) "google.at" LoginMethod {
    loginUrl = "https://accounts.google.com"
  , loginForm = undefined --LoginForm "" []
  , loginNeedToFiddleWithHTML = Just True
  , loginReq = \(Credentials (UId uid) (Password pw)) req jar -> do
        url <- parseUrl "https://accounts.google.com/ServiceLoginAuth"
        formDataBody [ partBS "GALX" $ fromJust $ lookup "GALX" (cookieJarToAsc jar)
                 , partBS "_utf8" "☃"
                 , partBS "bgresponse" "js_disabled"
                 , partBS "Email" (encodeUtf8 (T.pack uid))
                 , partBS "Passwd" (encodeUtf8 (T.pack pw))
                 , partBS "signIn" "Sign in"
                 , partBS "PersistentCookie" "yes"
                 , partBS "rmShown" "1"
                 ] url { cookieJar = Just jar }

          -- let domCookies = cookieToJSDOM <$> jar
          -- let cns = ["NID", "SID", "HSID", "SSID", "APISID", "SAPISID"]
          -- let cs = filter ((`elem` cns) . cookie_name) jar
          -- writeFile "domCookies" $ show $ cookieToJSDOM <$> cs
  }
 ]

credentials :: [((Site, UId), Secret)]
credentials = [
 (,) ("google.at", UId "smith@darkboxed.org") (Password "iN1qohQK45ATr3ygpYxC")
 ]

executeCommand :: Command -> IO CommandResponse
executeCommand (Login site uid ua) = do
  let Just lm  = lookup site loginMethods
      Just pw = lookup (site,uid) credentials
  Cookies <$> login lm (Credentials uid pw) ua

login :: LoginMethod -> Credentials -> String -> IO [Cookie]
login LoginMethod {..} crd ua = withManager tlsManagerSettings $ \mngr -> do
  -- First get the login page and all the cookies that go along with it
  req <- addUserAgent ua <$> parseUrl loginUrl
  withResponse' req mngr $ \(rs, (req',res')) -> do
      -- >/dev/null the response body for now (TODO: pass to loginReq)
      _ <- sequenceA $ brConsume <$> res'

      -- Do the actual login request
      lReq <- addUserAgent ua <$> (loginReq crd req' (responseCookieJar res'))
      withResponse' lReq mngr $ \(_, (req'', res'')) -> do
          res'' <- sequenceA $ brConsume <$> res''

          return $ evictOverwrittenCookies
                     $ destroyCookieJar $ responseCookieJar res''
 where
   addUserAgent ua req =
    req {
     requestHeaders = (hUserAgent, encodeUtf8 (T.pack ua)):requestHeaders req
   }

   bs :: BS.ByteString -> String
   bs s = T.unpack $ decodeUtf8 s
   url req = bs $ (host req) `BS.append` (path req) `BS.append` (queryString req)

   withResponse' req mngr f = do
       withResponseHistory req mngr $ \h -> do
           let history = hrRedirects h
               final = (hrFinalRequest h, hrFinalResponse h)
           putStr $ ((++"\n") . ("Request: "++) . url . fst) `concatMap` hrRedirects h
           f (history,final)
