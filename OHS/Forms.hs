{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NoMonomorphismRestriction #-}
-- | This is where the magic happens
module OHS.Forms where

import Data.List
import Text.XML.HXT.Core


-- just in case
import Text.Parsec
import Text.Parsec.String


import OHS.Types
