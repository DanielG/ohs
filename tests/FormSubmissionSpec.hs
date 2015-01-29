module FormSubmissionSpec where

import OHS.Types
import OHS.FormSubmission
import Data.List
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.ShowXml as XS
import Test.Hspec

html = "\
\<html>\
\<body>\
   \<form action=\"/root\" id=\"f1\">\
     \<form action=\"/anywhere\" id=\"f2\">\
       \<input type=\"text\" id=\"i1\"/>\
     \</form>\
     \<form action=\"/somewhere_else\" id=\"f3\">\
       \<input type=\"password\" id=\"i2\" />\
       \<input type=\"date\" form=\"f1\" id=\"i3\"/>\
     \</form>\
   \</form>\
\</body>\
\</html>"

spec =
 describe "Forms" $ do
   specResolveFormOwners
   specIsControlDisabled


specResolveFormOwners = it "have correctly resolved owner" $ do
     let html = "\
\<html>\
\<body>\
   \<form action=\"/root\" id=\"f1\">\
     \<form action=\"/anywhere\" id=\"f2\">\
       \<input type=\"text\" id=\"i1\"/>\
     \</form>\
     \<form action=\"/somewhere_else\" id=\"f3\">\
       \<fieldset id=\"s1\">\
         \<input type=\"password\" id=\"i2\" />\
         \<input type=\"date\" form=\"f1\" id=\"i3\"/>\
         \<input type=\"date\" form=\"doesNotExist\" id=\"i4\"/>\
       \</fieldset>\
     \</form>\
   \</form>\
\</body>\
\</html>"

     let rfo =
              hread >>> addNav
              >>>
              resolveFormOwners
              >>>
              first (remNav >>> getAttrValue0 "id") >>>
              second (remNav >>> getAttrValue0 "id")

     flip runLA html rfo `shouldBe`
        [ ("i1", "f2")
        , ("s1", "f3")
        , ("i2", "f3")
        , ("i3", "f1") -- can set form owner using form=""
        , ("i4", "f3") -- defaults to ancestor form if invalid id
        ]

specIsControlDisabled = it "knows when a field is disabled" $ do
  let html = "\
\<html>\
\<input disabled id=\"i1\">\
\<fieldset id=\"s1\" disabled>\
  \<input id=\"i2\">\
  \<input id=\"i3\">\
\</fieldset>\
\<fieldset id=\"s2\">\
  \<input id=\"i4\">\
  \<input id=\"i5\">\
\</fieldset>\
\<input id=\"i6\">\
\</html>"

  let t = addNav >>> (multi isControlDisabled >>> getId)

  runH html t `shouldBe` ["i1", "s1", "i2", "i3"]



runH h a = flip runLA h $ hread >>> a
getId = getAttrValue0 "id"
hasId id = hasAttrValue "id" (==id)
