{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NoMonomorphismRestriction,
  ConstraintKinds, FlexibleContexts, StandaloneDeriving #-}
-- | This is where the magic happens
module OHS.FormSubmission where

import qualified Control.Category as Cat

import Control.Monad (mplus)
import Data.List
import Data.Maybe
import Data.Tree.NavigatableTree.Class (NavigatableTree)
import Data.Tree.NTree.Zipper.TypeDefs (NTZipper (..), NTCrumb (..))
import qualified Data.Tree.NavigatableTree.XPathAxis as NT
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.XmlNode (XmlNode)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.ShowXml as XS
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import qualified Control.Category

-- just in case
import Text.Parsec
import Text.Parsec.String


import OHS.Types
import OHS.HXTrace as T
import OHS.FormSubmission.Types

import Debug.Trace

formReq :: (NavTree t, Eq (t XNode))
        => (t XNode)
        -> (FormSubmissionAttributes, [(String, Maybe String, SubmittableElement (t XNode))])
        -> Request
formReq t = let
--    (FormSubmissionAttributes {..}, dataset) = flip runLA t formSubmission
  in
    undefined

-- https://html.spec.whatwg.org/multipage/forms.html#form-submission-algorithm
formSubmission :: (ArrowXml a, NavTree t, Eq (t XNode))
               => a (t XNode) (FormSubmissionAttributes,
                               [((String, Maybe String), SubmittableElement (t XNode))])
formSubmission =
    getFormSubmissionAttributes &&& listA constructFormDataSet

-- https://html.spec.whatwg.org/multipage/forms.html#default-button
defaultButton :: (ArrowXml a, NavTree t)
              => a (t XNode) (t XNode)
defaultButton = undefined


-- https://html.spec.whatwg.org/multipage/forms.html#constructing-the-form-data-set
constructFormDataSet :: forall a t z. (ArrowXml a, NavTree t, Eq (t XNode))
                     => a (t XNode) ((String, Maybe String), SubmittableElement (t XNode))
constructFormDataSet =
    formControls >>> neg (hasDataListAncestor
                  &&& isControlDisabled
                  &&& isInvalidImageButton
                  &&& isObject)
             -- TODO: ignore checkbox, radio button in not checkedness state at
             -- later stage

             >>> fixImageButton `when` isImageButton
             >>> choiceA [
                      isSelect  :-> handleSelect,
                      isBoolean :-> handleBoolean,
                      isFile    :-> handleFile,
                      isObject  :-> handleObject,
                      this      :-> handleGeneric
                     ]

 where
   hasDataListAncestor = ancestorAxis >>> hasName "datalist"
   isInvalidImageButton = filterA $
       (enumAttr "type" >>> isA (==ImageButton))
         &&&
       (getAttrValue0 "name" >>> (isA (=="")))

   input :: a (t XNode) InputType
   input = tagOneOf ["input"] >>> enumAttr "type"

   isRadio     = filterA $ input >>> isA (==RadioButton)
   isCheckbox  = filterA $ input >>> isA (==Checkbox)
   isBoolean   = isRadio <+> isCheckbox
   isFile      = filterA $ input >>> isA (==FileUpload)
   isImageButton = filterA $ input >>> isA (==ImageButton)
   isObject = tagOneOf ["object"]
   isSelect = tagOneOf ["select"]

   -- TODO
   fixImageButton = arr (error "fixImageButton: TODO")
   handleSelect   = arr (error "handleSelect: TODO")
   handleBoolean  = arr (error "handleBoolean: TODO")
   handleFile     = arr (error "handleFile: TODO")
   handleObject   = arr (error "handleObject: TODO")

   handleGeneric = ((getAttrValue0 "name"
                    &&& arrToMaybe (getAttrValue0 "value"))
                    &&& submittableElement)

   submittableElement :: a (t XNode) (SubmittableElement (t XNode))
   submittableElement =
       choiceA [
        tag "button"   :-> (enumAttr "type" &&& this >>> arr2 SubmittableButton),
        tag "input"    :-> (enumAttr "type" &&& this >>> arr2 SubmittableInput),
        tag "keygen"   :-> arr SubmittableKeygen,
        tag "object"   :-> arr SubmittableObject,
        tag "select"   :-> arr SubmittableSelect,
        tag "textarea" :-> arr SubmittableTextarea,

        tagOneOf submittableTags :-> (getName >>> (arr $ \n ->
            error $ "submittableElement: unhandled element: " ++ n))
       ]
     where tag t = tagOneOf [t]

-- | Given a form element find all reassociable elements which are owned by this
-- form. (Will navigate to root)
formControls :: (ArrowXml a, NavTree t, Eq (t XNode)) => a (t XNode) (t XNode)
formControls = withA moveToRoot $
   first resolveFormOwners
   >>> isA (\((_,formOwner), form) -> form == formOwner)
   >>> first (arr fst)

 where
   withA :: Arrow a => a b c -> a (b, c) (d, c) -> a b d
   withA w a = (Control.Category.id &&& w) >>> a >>> arr fst

-- https://html.spec.whatwg.org/multipage/forms.html#concept-fe-disabled
isControlDisabled :: (ArrowXml a, Tree t, NavigatableTree t)
                  => a (t XNode) (t XNode)
isControlDisabled =
    (tagOneOf ["button", "input", "select", "textarea"] >>> hasAttr "disabled")
      `orElse`
    ((ancestorOrSelfAxis >>> hasName "fieldset" >>> hasAttr "disabled") `guards` this)


getFormSubmissionAttributes :: (ArrowXml a, NavTree t)
                            => a (t XNode) FormSubmissionAttributes
getFormSubmissionAttributes =
    hasName "form" >>>
      (parseURIAttr "action" &&&
      enumAttr "enctype" &&&
      enumAttr "method" &&&
      boolA (hasAttr "novalidate"))
    >>> arr4 FormSubmissionAttributes

 where
   parseURIAttr name = getAttrValue0 name >>> arrL (maybeToList . parseURI)

-- https://html.spec.whatwg.org/multipage/forms.html#category-submit
submittableTags =
    [ "button", "input", "keygen", "object", "select", "textarea" ]

-- https://html.spec.whatwg.org/multipage/forms.html#category-reset
reassociableTags =
    [ "button", "fieldset", "input", "keygen", "label", "object", "output"
    , "select", "textarea" ]

type NavTree t = (Tree t, NavigatableTree t)

-- https://html.spec.whatwg.org/multipage/forms.html#reset-the-form-owner
resolveFormOwners :: forall a t. (ArrowXml a, NavTree t)
                  => a (t XNode) (t XNode, t XNode)
resolveFormOwners = let
    idAList :: a (t XNode) [(String, (t XNode))]
    idAList = listA $ moveToRoot >>> deep (getAttrValue0 "id" &&& Cat.id)

    lookupId :: a (String, [(String, t XNode)]) (t XNode)
    lookupId = (arrL $ \(i, alist) -> maybeToList $ lookup i alist)

    ancestorForm :: a (t XNode) (t XNode)
    ancestorForm =
        single $ ancestorAxis >>> hasName "form"
 in
  ( multi (tagOneOf reassociableTags) &&& idAList) >>>
  ( arr fst &&& ((first (getAttrValue0 "form") >>> lookupId) `orElse` (arr fst >>> ancestorForm) ) )

-- where twace str a = Debug.Trace.trace (str ++ ": " ++ show (second (return >>> XS.xshow) `map` a)) a


tagOneOf l = hasNameWith ((`elem` l) . localPart)

enumAttr :: (ArrowXml a, EnumeratedAttribute c, Tree t) => String -> a (t XNode) c
enumAttr name =
    (getAttrValue0 name >>> arr toEnumAttrib) `orElse` constA missingDefault

 where
   toEnumAttrib :: EnumeratedAttribute a => String -> a
   toEnumAttrib s =
       case enumerateAttribute s of
         Just e -> e
         Nothing -> maybe (error $ "toEnumAttrib: Nothing: " ++ s) id invalidDefault -- TODO


arrToMaybe a = listA a >>> arr listToMaybe
boolA a = a >. (not . null)

deriving instance Eq a => Eq (NTZipper a)
deriving instance Eq a => Eq (NTCrumb a)
