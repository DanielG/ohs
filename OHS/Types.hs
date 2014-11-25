{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module OHS.Types where

import Control.Applicative
import Data.Maybe
import Data.Time
import Data.Time.ISO8601
import Data.Serialize
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Client
import Network.Socket.ByteString.Lazy

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

      -- | This function is called as: @loginReq req jar@ where @req@ is the
      -- last request in the redirect chain for the 'loginUrl' for this method
      -- and @jar@ is the response's cookie jar.
      loginReq :: (Request -> CookieJar -> IO Request)
    }

type URI = String
type DIV = String
type ID  = String

data LoginForm = LoginForm {
      loginFormId     :: (Maybe ID, Maybe DIV)
    , loginFormAction :: URI
    , loginFormMethod :: StdMethod
    , loginFormFields :: [String]
    }

data Command = Login {
      loginSite      :: String
    , loginUserAgent :: String
    } deriving (Show,Read,Generic)

instance Serialize Command

data CommandResponse = Cookies [Cookie] deriving (Show,Read,Generic)
instance Serialize CommandResponse

-- Evil, evil, evil, eviL
deriving instance Generic Cookie
instance Serialize Cookie

instance Serialize UTCTime where
    put t = put $ formatISO8601Picos t
    get = fromJust . parseISO8601 <$> get
