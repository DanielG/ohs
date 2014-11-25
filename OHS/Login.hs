module OHS.Login where

addUserAgent ua req = req { requestHeaders =
                              (hUserAgent, webKit_userAgent):requestHeaders req
                          }


data LoginMethod = LoginMethod {
      loginUrl :: String,
      loginReq :: (CookieJar -> IO Request)
    }


googleAccountsLogin = LoginMethod "https://accounts.google.com" $ \jar -> do
    url <- parseUrl "https://accounts.google.com/ServiceLoginAuth"

    formDataBody [ partBS "GALX" $ fromJust $ lookup "GALX" (cookieJarToAsc jar)
                 , partBS "_utf8" "☃"
                 , partBS "bgresponse" "js_disabled"
                 , partBS "Email" "smith@darkboxed.org"
                 , partBS "Passwd" "iN1qohQK45ATr3ygpYxC"
                 , partBS "signIn" "Sign in"
                 , partBS "PersistentCookie" "yes"
                 , partBS "rmShown" "1"
                 ] url { cookieJar = Just jar }

snapExampleLogin = LoginMethod "http://127.0.0.1:8000" $ \jar -> do
    req <- parseUrl "http://127.0.0.1:8000/login"

    return $ urlEncodedBody
               [ ("login", "example")
               , ("password", "example")
               ] req { cookieJar = Just jar }


main = doLogin -- server


doLogin = withManager tlsManagerSettings $ \mngr -> do
  let l = googleAccountsLogin
  req <- ua <$> parseUrl (loginUrl l)
  withResponse' req mngr $ \(rs, (req',res')) -> do
      _ <- sequenceA $ brConsume <$> res'
      loginReq <- ua <$> (loginReq l $ (responseCookieJar res'))
      withResponse' loginReq mngr $ \(_, (req'', res'')) -> do
          res'' <- sequenceA $ brConsume <$> res''

          let jar = evictOverwrittenCookies
                    $ destroyCookieJar $ responseCookieJar res''

          let domCookies = cookieToJSDOM <$> jar

          putStrLn ""

          let cns = ["NID", "SID", "HSID", "SSID", "APISID", "SAPISID"]
          let cs = filter ((`elem` cns) . cookie_name) jar
          writeFile "domCookies" $ show $ cookieToJSDOM <$> cs

 where
   infixr 9 .||.
   (.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
   f .||. g = \x -> f x || g x

   bs :: BS.ByteString -> String
   bs s = T.unpack $ decodeUtf8 s
   url req = bs $ (host req) `BS.append` (path req) `BS.append` (queryString req)

   withResponse' req mngr f = do
       withResponseHistory req mngr $ \h -> do
           let history = hrRedirects h
               final = (hrFinalRequest h, hrFinalResponse h)
           putStr $ ((++"\n") . ("Request: "++) . url . fst) `concatMap` hrRedirects h

           f (history,final)

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
