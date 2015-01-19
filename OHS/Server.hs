{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module OHS.Server where

import Control.Applicative
import Control.Exception
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.Text as T
import Network.URI (URI (..), URIAuth(..))
import Network.Simple.TCP
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Pipes as P
import Pipes.Network.TCP as PT
import Pipes.Aeson.Unchecked as PA (encode)
import Web.Cookie

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import qualified Data.Text as T

import Data.Maybe

import OHS.Types
import OHS.Cookies
import OHS.Pipes
import OHS.HTTP

inspectRes r = do
--  putStrLn $ "response: " ++ ( (cookies j)))

  (\c -> putStrLn $ "SetCookieI: " ++ jbs c) `mapM_` cookies r

  return r

jbs j = T.unpack $ decodeUtf8 $ LBS.toStrict $ Data.Aeson.encode j

server :: String -> String -> IO ()
server host port = serve (Host host) port $ \(s,_addr) -> cleanup s $ do
        let
          p = decodeJSON (PT.fromSocket s 4096)
                >-> ignoreDecodeFail
                >-> (await >>= (\j -> liftIO (putStrLn $ "request: " ++ show j) >> return j) >>= yield)
                >-> (await >>= (liftIO . executeCommand) >>= yield)
                >-> (await >>= liftIO . inspectRes >>= yield)
                >-> P.for cat PA.encode
                >-> toSocket s
        runEffect p
 where
  cleanup s = flip finally (closeSock s)

loginMethods :: [(SiteUrl, LoginMethod)]
loginMethods = [
 (,) "www.google.at" LoginMethod {
    loginUrl = "https://accounts.google.com"
  , loginNeedToFiddleWithHTML = Just True
  , loginReq = \(Credentials (UId uid) (Password pw)) _req jar -> do
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
  }
 ]

credentials :: [((SiteUrl, UId), Secret)]
credentials = [
 (,) ("www.google.at", UId "smith@darkboxed.org") (Password "iN1qohQK45ATr3ygpYxC")
 ]

executeCommand :: Command -> IO CommandResponse
executeCommand (Login uri uid ua) = do
  let Just dom = uriRegName <$> uriAuthority uri
      Just lm = lookup dom loginMethods
      Just pw = lookup (dom,uid) credentials

  uncurry LoginSuccess <$> login lm (Credentials uid pw) ua

executeCommand (Register site locator _cookies) = do
  undefined

login :: LoginMethod -> Credentials -> String -> IO (URI, [SetCookie])
login LoginMethod {..} crd ua = withManager uaManagerSettings $ \mngr -> do
  -- First get the login page and all the cookies that go along with it
  req <- parseUrl loginUrl

  withBrowser mngr req $ \((req',res'):_) -> do
      (\c -> putStrLn $ "SetCookie': " ++ jbs (cookieToSetCookie c)) `mapM_` extractCookies res'

      -- Do the actual login request
      lReq <- loginReq crd req' (responseCookieJar res')

      withBrowser mngr lReq $ \((req'',res''):_) -> do
          let
              targetUri = getUri req''
              cookies = extractCookies res''

          return $ (targetUri, map cookieToSetCookie cookies)
 where
   extractCookies =
       evictOverwrittenCookies . destroyCookieJar . responseCookieJar
   uaManagerSettings = managerUserAgent ua tlsManagerSettings

   bs :: BS.ByteString -> String
   bs s = T.unpack $ decodeUtf8 s
   url req = bs $ (host req) `BS.append` (path req) `BS.append` (queryString req)
