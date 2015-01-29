{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module OHS.FormSubmission.Types where

import qualified Data.CaseInsensitive as CI
import Network.URI

-- https://html.spec.whatwg.org/multipage/infrastructure.html#enumerated-attribute
class EnumeratedAttribute a where
    missingDefault :: a
    invalidDefault :: Maybe a
    enumerateAttribute :: String -> Maybe a

data Method = GET | POST | Dialog
    deriving (Show, Read, Eq)

instance EnumeratedAttribute Method where
    missingDefault = GET
    invalidDefault = Just GET
    enumerateAttribute s
        | CI.mk s == "get" = Just GET
        | CI.mk s == "post" = Just POST
        | CI.mk s == "dialog" = Just Dialog
        | otherwise = Nothing

data EncType = UrlEncoded | Multipart | TextPlain

instance EnumeratedAttribute EncType where
    missingDefault = UrlEncoded
    invalidDefault = Just UrlEncoded
    enumerateAttribute s
        | CI.mk s == "application/x-www-form-urlencoded" = Just UrlEncoded
        | CI.mk s == "multipart/form-data" = Just Multipart
        | CI.mk s == "text/plain" = Just TextPlain
        | otherwise = Nothing


-- https://html.spec.whatwg.org/multipage/forms.html#attr-input-type
data InputType = Hidden
               | Text
               | Search
               | Telephone
               | URL
               | Email
               | Password
               | DateAndTime
               | Date
               | Month
               | Week
               | Time
               | LocalDateAndTime
               | Number
               | Range
               | Colour
               | Checkbox
               | RadioButton
               | FileUpload
               | SubmitButton
               | ImageButton
               | ResetButton
               | ButtonButton
                 deriving (Show, Read, Eq)

instance EnumeratedAttribute InputType where
    missingDefault = Text
    invalidDefault = Nothing
    enumerateAttribute s
        | CI.mk s == "text"            = Just Text
        | CI.mk s == "search"          = Just Search
        | CI.mk s == "tel"             = Just Telephone
        | CI.mk s == "url"             = Just URL
        | CI.mk s == "email"           = Just Email
        | CI.mk s == "password"        = Just Password
        | CI.mk s == "datetime"        = Just DateAndTime
        | CI.mk s == "date"            = Just Date
        | CI.mk s == "month"           = Just Month
        | CI.mk s == "week"            = Just Week
        | CI.mk s == "time"            = Just Time
        | CI.mk s == "datetime-local"  = Just Telephone
        | CI.mk s == "number"          = Just Number
        | CI.mk s == "range"           = Just Range
        | CI.mk s == "color"           = Just Colour
        | CI.mk s == "checkbox"        = Just Checkbox
        | CI.mk s == "radio"           = Just RadioButton
        | CI.mk s == "file"            = Just FileUpload
        | CI.mk s == "submit"          = Just SubmitButton
        | CI.mk s == "image"           = Just ImageButton
        | CI.mk s == "reset"           = Just ResetButton
        | CI.mk s == "button"          = Just ButtonButton
        | otherwise                    = Nothing

data ButtonType = Submit
                | Reset
                | Button
                | Menu
                  deriving (Show, Read, Eq)

instance EnumeratedAttribute ButtonType where
    missingDefault = Submit
    invalidDefault = Nothing
    enumerateAttribute s
        | CI.mk s == "submit" = Just Submit
        | CI.mk s == "reset"  = Just Reset
        | CI.mk s == "button" = Just Button
        | CI.mk s == "Menu"   = Just Button
        | otherwise           = Nothing

data SubmittableElement a = SubmittableButton ButtonType a
                          | SubmittableInput InputType a
                          | SubmittableKeygen a
                          | SubmittableObject a
                          | SubmittableSelect a
                          | SubmittableTextarea a
                            deriving (Show, Read, Eq, Functor)

-- https://html.spec.whatwg.org/multipage/forms.html#form-submission
data FormSubmissionAttributes = FormSubmissionAttributes {
      fsAction      :: URI --valid non-empty URL potentially surrounded by spaces
    , fsEncType     :: EncType
    , fsMethod      :: Method
    , fsNoValidate  :: Bool
--    , fsTarget      :: () -- Don't think we need this
    }
