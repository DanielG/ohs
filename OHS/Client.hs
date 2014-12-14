module OHS.Client (
    module OHS.Client
  , SiteUrl
  , UId (..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson
--import Data.Serialize (encode, decodeLazy)
import Network.Socket.ByteString
import Network.Simple.TCP
import Network.HTTP.Client
import qualified Network.Socket.ByteString.Lazy as NBL
import OHS.Types

data OHSEnv = OHSEnv {
      clSessSocket   :: Socket
    , clSessSockAddr :: SockAddr
    }

type OHST = ReaderT OHSEnv

runOHST :: (MonadIO m, MonadMask m) => String -> String -> (OHST m a) -> m a
runOHST host port action =
    connect host port $ \(sock, addr) -> runReaderT action (OHSEnv sock addr)

login :: (MonadIO m, MonadMask m) => SiteUrl -> UId -> OHST m [Cookie]
login site uid = do
  OHSEnv sock addr <- ask
  liftIO $ NBL.sendAll sock $ encode Login {
                   loginSite = site
                 , loginUserId = uid
                 , loginUserAgent = "A random user's agent"
                 }
-- TODO
  res <- liftIO $ NBL.getContents sock
  case decode' res of
    Just (Cookies c) -> return c
    Nothing -> fail "decode"
