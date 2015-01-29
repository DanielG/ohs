{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module OHS.HTTP where

import Network.URI (URI, parseURI, parseURIReference, relativeTo, uriToString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Header

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import Data.Maybe
import Data.List
import Data.Traversable
import Data.Text.Encoding
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.String
import Text.XML.HXT.Core

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI

import OHS.Cookies
import OHS.Types


managerUserAgent :: String -> ManagerSettings -> ManagerSettings
managerUserAgent ua settings = settings {
    managerModifyRequest = \req -> do
      req' <- managerModifyRequest settings req
      return $ addUserAgent ua req'
   }
 where
   addUserAgent ua req =
    req {
     requestHeaders = (hUserAgent, encodeUtf8 (T.pack ua)):requestHeaders req
   }

-- http://www.w3.org/TR/html5/document-metadata.html#attr-meta-http-equiv-refresh
httpEquivRefreshContentP :: Parser (Int, Maybe URI)
httpEquivRefreshContentP = do
  spaces
  time <- many digit
  _ <- many (digit <|> char '.')
  spaces
  oneOf ";,"

  murl <- optionMaybe $ do
    spaces
    oneOf "uU" >> oneOf "rR" >> oneOf "lL"
    spaces
    char '='
    spaces
    mquote <- optionMaybe $ oneOf "\"'"
    url <- between (oneOf "\"'") (oneOf "\"'") anyChar
    let url = filter (flip elem "\n\r\t") $ dropWhileEnd isSpace url
    return $ parseURIReference url

  return (read time, join murl)

arrP :: ArrowList a => Parser b -> (a String b)
arrP p = arrL $ \s ->
         case runParser p () "" s of
           Left err -> []
           Right x -> [x]

withBrowser :: Manager -> Request -> ( [(Request, Response ByteString)] -> IO b ) -> IO b
withBrowser manager req0 fn = do
  putStrLn $ "Request0: " ++ url req0
  go [] req0
 where
   go hist req = do
     withResponseHistory' req manager $ \(rs, (req',res'Reader)) -> do
          (\(req, res) -> do
               putStrLn $ "RequestH: " ++ url req
               (\c -> putStrLn $ "SetCookieH: " ++ jbs (cookieToSetCookie c)) `mapM_` extractCookies res
               putStrLn ""
            ) `mapM_` rs

          putStrLn $ "Request: " ++ url req'
          (\c -> putStrLn $ "SetCookie: " ++ jbs (cookieToSetCookie c)) `mapM_` extractCookies res'Reader
          putStrLn ""

          res' <- fmap BS.concat <$> (sequenceA $ brConsume <$> res'Reader)

          let res'Body = responseBody res'
              mredir = join $ fmap snd $ listToMaybe $
                         flip runLA (fromUTF8BS $ res'Body) $
                              hread >>> deep (hasName "head") >>> metaRedirect

          let rs' = second (fmap LBS.toStrict) `map` rs
              hist' = concat [(req',res'):rs', hist]

          case mredir of
            Just redir -> do
               let
                 redirURI = redir `relativeTo` getUri req'
                 Just redirReq = parseUrl $ uriToString id redirURI ""

               putStrLn $ "Meta Request: " ++ url redirReq
               go hist' redirReq

            Nothing -> do
              putStrLn "Done"
              fn hist'

   jbs j = T.unpack $ decodeUtf8 $ LBS.toStrict $ Data.Aeson.encode j

   extractCookies =
       evictOverwrittenCookies . destroyCookieJar . responseCookieJar

   metaRedirect =
       deep (hasName "meta")
       >>> hasAttrValue "http-equiv" ((=="Refresh") . CI.mk)
       >>> getAttrValue0 "content" >>> arrP httpEquivRefreshContentP

   addUserAgent ua req =
    req {
     requestHeaders = (hUserAgent, encodeUtf8 (T.pack ua)):requestHeaders req
   }

   fromUTF8BS :: BS.ByteString -> String
   fromUTF8BS s = T.unpack $ decodeUtf8 s

   url req = fromUTF8BS $
       (host req) `BS.append` (path req) `BS.append` (queryString req)

   withResponseHistory' req mngr f = do
       withResponseHistory req mngr $ \h ->
           let history = hrRedirects h
               final = (hrFinalRequest h, hrFinalResponse h)
           in f (history,final)


--           putStr $ ((++"\n") . ("Request: "++) . url . fst) `concatMap` hrRedirects h
