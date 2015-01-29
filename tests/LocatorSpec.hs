module LocatorSpec where

import OHS.Types
import OHS.FormSubmission
import Data.List
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.ShowXml as XS
import Test.Hspec


spec = describe "Locator" $ do
  return ()

referenceSitesHaventChanged = undefined

runLArrow arrow = runLA (xshow (hread >>> arrow >>> indentDoc))

someLocator = [
      LocatorNode "html" 0 (Just "id1") []
    , LocatorNode "body" 1 (Just "id2") []
    , LocatorNode "form"    0 (Just "id3") ["box", "important", "pretty"]
    ]

someHtml = "<html><head></head><body><a><b><c>\
           \<form id=\"id3\" class=\"pretty red imporant\">Hello world</form>\
           \</c></b></a><z class=\"important box\">OMG BY OUR SHOES</z></body></html>"


foo :: (ArrowXml a) => a XmlTree XmlTree
foo = deep (hasName "form")

-- test = do
--   let doc = readString[withParseHTML yes, withWarnings no] someHtml
--   res <- (runX $ doc >>> locate someLocator)

--   let sres = map (second (XS.xshow . (:[]))) res
--       nres = sort >>> group >>> map (length &&& head) >>> sortBy (\a b -> fst a `compare` fst b) $ sres

--   return nres

-- test2 :: [Locator]
-- test2 = runLA (hread >>> locatorFromId "id3") someHtml


-- id: "SignInForm"
