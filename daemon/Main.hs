{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Either
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Format
import Data.Traversable
import Data.Word
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Network.HTTP.Client.Internal

import System.Locale

import OHS.Types
import OHS.Server

webKit_userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/538.15 (KHTML, like Gecko) Version/8.0 Safari/538.15"

main = server "127.0.0.1" "1234"
