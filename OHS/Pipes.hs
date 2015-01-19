{-# OPTIONS -Wall #-}
module OHS.Pipes (
    module OHS.Pipes
  ) where

import Control.Applicative
import Control.Monad.State.Strict (runStateT)
import Data.Serialize hiding (get, put)
import Data.Aeson
import Pipes as P
import Pipes.Aeson as PA
--import Pipes.Aeson.Unchecked as PA (encode)

import qualified Data.ByteString as BS

decoderPipe :: MonadIO m => Get a -> Pipe BS.ByteString a m ()
decoderPipe get = go (runGetPartial get) BS.empty
 where
   go f bs =
       case f bs of
         Fail err _rest -> fail err
         Partial f' -> await >>= go f'
         Done r rest -> yield r >> go (runGetPartial get) rest

encodePipe :: Monad m => Putter a -> Pipe a BS.ByteString m ()
encodePipe put =
  yield =<< (runPut . put) <$> await

ignoreDecodeFail :: Monad m => Pipe (Maybe (Either DecodingError a)) a m ()
ignoreDecodeFail = do
  Just (Right a) <- await
  yield a

decodeJSON :: (Monad m, FromJSON a)
           => Producer BS.ByteString m ()
           -> Producer (Maybe (Either DecodingError a)) m ()
decodeJSON prod = do
  (r,l) <- lift $ runStateT PA.decode prod
  yield r
  decodeJSON l
