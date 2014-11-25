{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.String
import Network.HTTP.Client
import System.Posix.Directory
import System.Glib.GType

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
import Graphics.UI.Gtk.WebKit.DOM.HTMLScriptElement
import Graphics.UI.Gtk.WebKit.DOM.EventM
import Graphics.UI.Gtk.WebKit.DOM.UIEvent
import Graphics.UI.Gtk.WebKit.DOM.MouseEvent



webScrolledWindow wv = do
  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` wv
  return sw

addressBar w wv uri = do
  addressBar <- entryNew
  entrySetText addressBar uri

  webViewLoadUri wv uri
  onEntryActivate addressBar $ do
    uri' <- entryGetText addressBar :: IO String
    webViewLoadUri wv uri'

  wv `on` loadCommitted $ \frame -> do
    muri' <- webFrameGetUri frame :: IO (Maybe String)
    case muri' of
      Just uri' -> entrySetText addressBar uri'
      Nothing  -> return ()

  return addressBar

browserVBox ab sw = do
  box <- vBoxNew False 0
  boxPackStart box ab PackNatural 0
  boxPackStart box sw PackGrow 0
  return box

browserWindow uri = do
  w <- windowNew
  wv <- webViewNew
  sw <- webScrolledWindow wv
  ab <- addressBar w wv uri

  box <- browserVBox ab sw

  w `containerAdd` box
  w `onDestroy` mainQuit

  return (w,wv,ab)

showTheFuckingWebInspector wv = do
  ws <- webViewGetWebSettings wv
  set ws [ webSettingsEnableDeveloperExtras := True ]
  webViewSetWebSettings wv ws

  i <- webViewGetInspector wv
  on i inspectWebView $ \_iwv -> do
    iwv <- webViewNew
    iw <- webScrolledWindow iwv
    widgetShowAll iw
    return iwv
  webInspectorShow i

withMainGUI action = do
  _ <- initGUI
  w <- action
  widgetShowAll w
  mainGUI

main = withMainGUI $ do
  cwd <- getWorkingDirectory
  (w,wv,ab) <- browserWindow "http://google.at"

  domCookies <- read <$> readFile "domCookies" :: IO [String]

  withDocument wv $ \d -> do
    putStrLn . ("Cookies: "++) =<< documentGetCookie d

  withDocumentOnce wv $ \d -> do
    putStrLn . ("Cookies before injection: "++) =<< documentGetCookie d

    putStrLn $ "Cookies to be injected:\n>>>>>\n" ++ jsSetDomCookies domCookies ++ "<<<<"

    webViewExecuteScript wv $ (jsSetDomCookies domCookies)
    webViewReloadBypassCache wv



  return w



--          _ <- documentOnsubmit d $ do
--                 liftIO $ putStrLn "documentOnsubmit"
--          _ <- documentOnclick d $ do
--                 ctrl <- mouseCtrlKey
--                 coords@(x,y) <- liftM2 (,) uiPageX uiPageY
--                 liftIO $ do
--                   Just el <- documentElementFromPoint d x y
-- --                  putStrLn $ typeName $ typeFromInstance n
--                   nn <- nodeGetNodeName el
--                   t <- nodeGetTextContent el
--                   putStrLn ("textContent: " ++ t)
--                   putStrLn ("nodename: " ++ nn)
--                   print coords


jsSetDomCookies :: [String] -> String
jsSetDomCookies cs =
    concat $ map (\c -> "document.cookie = \"" ++ c ++ "\";\n") cs

jsReload :: String
jsReload = "window.location.reload(true);\n"


withDocumentOnce :: WebView -> (Document -> IO ()) -> IO ()
withDocumentOnce wv action = void $ once wv documentLoadFinished $ \mask _ -> do
                    mask
                    Just d <- webViewGetDomDocument wv
                    action d


withDocument :: WebView -> (Document -> IO ()) -> IO ()
withDocument wv action = void $ on wv documentLoadFinished $ \_ -> do
                    Just d <- webViewGetDomDocument wv
                    action d


injectScript :: Document -> String -> IO ()
injectScript d src = do
         Just body <- documentGetBody d
         Just sc' <- documentCreateElement d "script"
         let sc = castToHTMLScriptElement sc'
         htmlScriptElementSetText sc src
         nodeAppendChild body (Just sc)
         return ()

once :: forall object callback. GObjectClass object => object
     -> Graphics.UI.Gtk.Signal object callback
     -> (IO () -> callback)
     -> IO ()
once object signal action = do
  ref <- newIORef undefined :: IO (IORef (ConnectId object))
  cid <- on object signal (action (readIORef ref >>= signalDisconnect))
  writeIORef ref cid
