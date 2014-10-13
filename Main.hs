import Control.Applicative
import Control.Monad.Trans
import Control.Monad

import System.Posix.Directory

import System.Glib.GType

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
import Graphics.UI.Gtk.WebKit.DOM.HTMLScriptElement
import Graphics.UI.Gtk.WebKit.DOM.EventM
import Graphics.UI.Gtk.WebKit.DOM.UIEvent
import Graphics.UI.Gtk.WebKit.DOM.MouseEvent

main = do
  _ <- initGUI
  w  <- windowNew


  sw <- scrolledWindowNew Nothing Nothing

  vb <- vBoxNew False 2
  wv <- webViewNew
  tv <- textViewNew

--  containerAdd vb tv
--  containerAdd vb sw

  set sw [ containerChild := wv ]

  set w
      [ containerChild       := sw
      , windowDefaultWidth   := 500
      , windowDefaultHeight  := 400
      , containerBorderWidth := 2
      ]

  cwd <- getWorkingDirectory
  webViewLoadUri wv ("file://" ++ cwd ++ "/test.html")
--  webViewLoadUri wv "http://slashdot.org"

  onDestroy w mainQuit

  on wv documentLoadFinished $ \_ -> do
         putStrLn "documentLoadFinished"
         Just d <- webViewGetDomDocument wv

         Just body <- documentGetBody d

         Just sc' <- documentCreateElement d "script"
         let sc = castToHTMLScriptElement sc'
         htmlScriptElementSetText sc "console.log(\"hello world\");\ndocument.getElementById(\"text\").value = \"hello\";\n"
         nodeAppendChild body (Just sc)

         _ <- documentOnsubmit d $ do
                liftIO $ putStrLn "documentOnsubmit"
         _ <- documentOnclick d $ do
                ctrl <- mouseCtrlKey
                coords@(x,y) <- liftM2 (,) uiPageX uiPageY
                liftIO $ do
                  Just el <- documentElementFromPoint d x y
--                  putStrLn $ typeName $ typeFromInstance n
                  nn <- nodeGetNodeName el
                  t <- nodeGetTextContent el
                  putStrLn ("textContent: " ++ t)
                  putStrLn ("nodename: " ++ nn)
                  print coords


         return ()

  widgetShowAll w
  mainGUI
