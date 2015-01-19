{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module OHS.Client (
    module OHS.Client
  , SiteUrl
  , UId (..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
--import Data.Serialize (encode, decodeLazy)
import Network.URI
import Network.Socket.ByteString
import Network.Simple.TCP (Socket, SockAddr, connect)
import Network.HTTP.Client
import qualified Network.Socket.ByteString.Lazy as NBL
import qualified Pipes.Network.TCP as PT
import Web.Cookie

import OHS.Types

data OHSEnv = OHSEnv {
      clSessSocket   :: Socket
    , clSessSockAddr :: SockAddr
    }

type OHST m = StateT (Maybe ByteString) (ReaderT OHSEnv m)

runOHST :: (MonadIO m, MonadMask m) => String -> String -> (OHST m a) -> m a
runOHST host port action =
    connect host port $ \(sock, addr) ->
        runReaderT (evalStateT action Nothing) (OHSEnv sock addr)

login :: (MonadIO m, MonadMask m) => URI -> UId -> OHST m (URI, [SetCookie])
login uri uid = do
  res <- interaction Login {
                  loginURL = uri
                , loginUserId = uid
                , loginUserAgent = "A random user's agent"
                }

  case res of
    LoginSuccess target c -> return (target, c)
    CommandFail reason -> fail $ "login: command failed: " ++ show reason
    _ -> fail $ "login: unexpected response ("++show res++")"


interaction :: MonadIO m => Command -> OHST m CommandResponse
interaction cmd = sendCommand cmd >> recvResponse

sendCommand :: MonadIO m => Command -> OHST m ()
sendCommand cmd = do
  OHSEnv sock addr <- ask
  liftIO $ NBL.sendAll sock $ encode cmd

recvResponse :: MonadIO m => OHST m CommandResponse
recvResponse = ask >>= \(OHSEnv sock _) -> do
  mbs <- get
  (r, a) <- lift . go . (parse json') =<< case mbs of
                                          Just bs -> return bs
                                          Nothing -> liftIO (recv sock 4096)

  put r

  case fromJSON a of
    Error msg -> fail msg
    Success a' -> return a'

 where
   go :: (MonadReader OHSEnv m, MonadIO m)
      => IResult ByteString Value -> m (Maybe ByteString, Value)
   go (Fail _ ctx msg) = fail msg
   go (Partial cont) = do
     OHSEnv sock _ <- ask
     bs <- liftIO $ recv sock 4096
     go (cont bs)
   go (Done rest cmdRes)
       | BS.null rest = return (Nothing, cmdRes)
       | otherwise = return (Just rest, cmdRes)

register :: MonadIO m => OHST m ()
register = do
  OHSEnv sock addr <- ask

  res <- interaction mockRegRes

  case res of
    CommandSuccess -> liftIO $ putStrLn "woo"
    _ -> fail $ show res


mockRegRes = res
 where Just res = decodeStrict' $ BS8.pack "{\"regURL\":\"https://accounts.google.com/ServiceLogin?hl=de&continue=http://www.google.at/%3Fgfe_rd%3Dcr%26ei%3D5Fm1VLqINYuH8Qf-hoC4Ag\",\"regFormLocator\":[{\"lnId\":\"Email\",\"lnClasses\":null,\"lnTag\":\"INPUT\",\"lnOffset\":13},{\"lnId\":\"gaia_loginform\",\"lnClasses\":null,\"lnTag\":\"FORM\",\"lnOffset\":5},{\"lnId\":null,\"lnClasses\":\"card signin-card clearfix\",\"lnTag\":\"DIV\",\"lnOffset\":3},{\"lnId\":null,\"lnClasses\":\"main content clearfix\",\"lnTag\":\"DIV\",\"lnOffset\":3},{\"lnId\":null,\"lnClasses\":\"wrapper\",\"lnTag\":\"DIV\",\"lnOffset\":1},{\"lnId\":null,\"lnClasses\":null,\"lnTag\":\"BODY\",\"lnOffset\":2},{\"lnId\":null,\"lnClasses\":null,\"lnTag\":\"HTML\",\"lnOffset\":1},{}],\"regCookies\":[\"GoogleAccountsLocale_session=de\",\"GAPS=1:SJVkrP6nkNG-tXXR01AV6i-kO4Rqig:-z2lKv0OhyGZwVuP\",\"GALX=-BmfORdSPRA\"]}"
