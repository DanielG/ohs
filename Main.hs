import Control.Monad.Trans

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.WebKit.DOM.EventM
import Graphics.UI.Gtk.WebKit.DOM.UIEvent

main = do
  _ <- initGUI
  w  <- windowNew
  sw <- scrolledWindowNew Nothing Nothing
  wv <- webViewNew

  set w
      [ containerChild       := sw
      , windowDefaultWidth   := 500
      , windowDefaultHeight  := 400
      , containerBorderWidth := 2
      ]

  set sw [ containerChild := wv ]

  webViewLoadUri wv "test.html"

  onDestroy w mainQuit

  on wv documentLoadFinished $ \_ -> do
         putStrLn "documentLoadFinished"
         Just d <- webViewGetDomDocument wv
         _ <- documentOnsubmit d $ do
           liftIO $ putStrLn "documentOnsubmit"
         return ()

  widgetShowAll w
  mainGUI
