{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, StandaloneDeriving, DeriveGeneric #-}
module OHS.Types where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad
import Data.ByteString
import qualified Data.ByteString as BS
import Data.Default.Class
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.Time.ISO8601
import Data.String
import Data.Serialize
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Parser
import GHC.Generics
import Network.URI
import Network.HTTP.Types
import Network.HTTP.Client
import Network.Socket.ByteString.Lazy
import System.Locale
import Web.Cookie

type SiteUrl = String

data UId = UId String deriving (Eq,Show,Generic)
data Secret = Password String deriving (Eq,Show,Generic)
data Credentials = Credentials { crdUId    :: UId
                               , crdSecret :: Secret
                               }
                   deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Credentials)

instance ToJSON UId         where toJSON (UId x) = toJSON x
instance ToJSON Secret      where toJSON (Password x) = toJSON x

instance FromJSON UId         where parseJSON v = UId <$> parseJSON v
instance FromJSON Secret      where parseJSON v = Password <$> parseJSON v

data LoginMethod = LoginMethod {
      -- In the future we should find a way to classify whether a page is a
      -- login page for a certain domain rather than "hardcoding" the form's
      -- target url.

      -- Nope that doesn't work if we consider attacker controlled content on a
      -- site. Imagine some sort of file upload thing where the server just
      -- serves the attacker controlled content on a sub path. The attacker
      -- could then trick the server into submitting the password to an arbitary
      -- URL (because he can trick our classifier). Even if we check the target
      -- URL is within the same domain the site could still have some POST URL
      -- where the attacker is able to extract the password. So we really do
      -- need to hardcode this or at least find it by interacting with a trusted
      -- part of the site.

      -- Sites like Amazon make this really hard as their login forms are behind
      -- VERY long and complicated looking URLs that contain all sorts of
      -- information.

      -- Stripping away this information doesn't seem to work either the server
      -- returns an error.  .... :/
      loginUrl :: String,

      -- | Some sites use hidden fields to their login forms, so we have to do a
      -- request and parse the <form> to get all the login request parameters we
      -- need to send to the server. If this is It's 'Just' 'True' we do this,
      -- otherwise we dont. 'Nothing' means we haven't checked yet.
      loginNeedToFiddleWithHTML :: Maybe Bool,

      -- | This function is called as: @loginReq crd req jar@ where @crd@ are
      -- the login credentials, @req@ is the last request in the redirect chain
      -- for the 'loginUrl' for this method and @jar@ is the response's cookie
      -- jar.
      loginReq :: (Credentials -> Request -> CookieJar -> IO Request)
    }

type Locator = [LocatorNode]
data LocatorNode = LocatorNode {
      lnTag   :: String
      -- | Sibbling offset (0 is first)
    , lnOffset  :: Int
    , lnId      :: Maybe String
    , lnClasses :: [String]
    } deriving (Show, Generic)

instance Serialize LocatorNode
$(deriveJSON defaultOptions ''LocatorNode)


data Command =
      Login {
    -- | URL of the site the user wants to login to
      loginURL      :: URI

    -- | Email, username whatever depending on the site
    , loginUserId    :: UId

    -- | The frontend browser's user agent to make our requests indistinguishable
    , loginUserAgent :: String
    }

    | Register {
      regURL         :: String
    , regFormLocator :: Locator
    , regCookies     :: [String]
    }

  deriving (Show,Generic)
$(deriveJSON defaultOptions ''Command)

data FailReason = LoginFailed
                | NetworkError
  deriving (Show, Read, Generic)
$(deriveJSON defaultOptions ''FailReason)

data CommandResponse = CommandSuccess
                     | CommandFail { failReason :: FailReason }
                     | LoginSuccess { targetURL :: URI, cookies :: [SetCookie] }
  deriving (Show, Generic)
$(deriveJSON defaultOptions ''CommandResponse)


instance ToJSON URI where
    toJSON uri = String $ T.pack $ uriToString id uri ""

instance FromJSON URI where
    parseJSON (String s) = maybe mzero return $ parseURI $ T.unpack s

instance ToJSON SetCookie where
    toJSON sc = String $ decodeUtf8 $ toByteString $ renderSetCookie sc

instance FromJSON SetCookie where
    parseJSON (String s) = return $ parseSetCookie $ encodeUtf8 s

instance ToJSON ByteString where
    toJSON x = String (decodeUtf8 x)

instance FromJSON ByteString where
    parseJSON (String t) = return $ encodeUtf8 t
    parseJSON _ = fail "parseJSON ByteString: expecting a String"

instance Serialize UTCTime where
    put t = put $ formatISO8601Picos t
    get = fromJust . parseISO8601 <$> get
