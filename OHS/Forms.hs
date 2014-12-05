module OHS.Forms where

import Text.XML.HXT.Core

type Locator = [LocatorNode]
data LocatorNode = LocatorNode {
      lnTag   :: String
      -- | Sibbling offset (0 is first)
    , lnOffset :: Int
    , lnId    :: Maybe String
    , lnClass :: Maybe String
    } deriving (Read, Show)


foo :: String -> [String]
foo = runLA (xshow (hread >>> deep (hasName "form") >>> indentDoc))
