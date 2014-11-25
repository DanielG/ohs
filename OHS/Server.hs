{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module OHS.Server where

import Control.Exception
import Data.Serialize
import Network.Simple.TCP
import Network.Socket.ByteString.Lazy
import Network.Protocol.SASL.GNU
import Network.HTTP.Client

import OHS.Types

server :: String -> String -> IO ()
server host port = serve (Host host) port $ \(s,addr) -> cleanup s $ do
        bs <- Network.Socket.ByteString.Lazy.getContents s
        let (Right cs :: Either String [Command]) = decodeLazy bs
        rs <- executeCommand `mapM` cs
        sendAll s $ encodeLazy rs
 where
  cleanup s = flip finally (closeSock s)

executeCommand :: Command -> IO CommandResponse
executeCommand (Login _ _) = do
  return $ Cookies []
