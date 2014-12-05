{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveGeneric #-}
module OHS.Types where

import Control.Applicative
import Data.ByteString
import Data.Maybe
import Data.Text.Encoding
import Data.Time
import Data.Time.ISO8601
import Data.Serialize
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Parser
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Client
import Network.Socket.ByteString.Lazy

type Site = String

data UId = UId String deriving (Eq,Show,Read,Generic)
data Secret = Password String deriving (Eq,Show,Read,Generic)
data Credentials = Credentials { crdUId    :: UId
                               , crdSecret :: Secret
                               }
                   deriving (Eq,Show,Read,Generic)
$(deriveJSON defaultOptions ''Credentials)


instance Serialize UId
instance Serialize Secret
instance Serialize Credentials

instance ToJSON UId         where toJSON (UId x) = toJSON x
instance ToJSON Secret      where toJSON (Password x) = toJSON x

instance FromJSON UId         where parseJSON v = UId <$> parseJSON v
instance FromJSON Secret      where parseJSON v = Password <$> parseJSON v

data LoginMethod = LoginMethod {
      -- In the future we should find a way to classify whether a page is a
      -- login page for a certain domain rather than "hardcoding" the form's
      -- target url.
      loginUrl :: String,

      loginForm :: LoginForm,

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

type URI = String
type DIV = String
type ID  = String

data LoginForm = LoginForm {
      loginFormId     :: (Maybe ID, Maybe DIV)
    , loginFormAction :: URI
    , loginFormMethod :: StdMethod
    , loginFormFields :: [String]
    } deriving (Show,Read,Generic)

data Command = Login {
    -- | Domain of the site the user wants to login to
      loginSite      :: String

    -- | Email, username whatever depending on the site
    , loginUserId    :: UId

    -- | The frontend browser's user agent to make our requests indistinguishable
    , loginUserAgent :: String

    } deriving (Show,Read,Generic)

instance Serialize Command
$(deriveJSON defaultOptions ''Command)

data CommandResponse = Cookies [Cookie] deriving (Show,Read,Generic)
instance Serialize CommandResponse
$(deriveJSON defaultOptions ''CommandResponse)

-- Evil, evil, evil, eviL
deriving instance Generic Cookie
instance Serialize Cookie
$(deriveJSON defaultOptions ''Cookie)

instance ToJSON ByteString where
    toJSON x = String (decodeUtf8 x)

instance FromJSON ByteString where
    parseJSON (String t) = return $ encodeUtf8 t
    parseJSON _ = fail "parseJSON ByteString: expecting a String"

instance Serialize UTCTime where
    put t = put $ formatISO8601Picos t
    get = fromJust . parseISO8601 <$> get
