{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NoMonomorphismRestriction, FlexibleContexts #-}
-- | This is where the magic happens
module OHS.Locator where

import Safe
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Aeson.TH
import Text.XML.HXT.Core
import Control.Applicative
import Control.Arrow.ListArrow
import Control.Arrow.ArrowNavigatableTree
import Data.Tree.NavigatableTree.Class
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.Zipper.TypeDefs

import qualified Control.Category as Cat
import qualified Data.Tree.Class as T
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.ShowXml as XS

import qualified OHS.HXTrace as Tr

import OHS.Types

-- | Traverse every possible path in the input tree based on the given
-- 'Locator'. The locator is a "path" through the XML tree recorded in as many
-- diverse ways as possible. Namely it consists of element tags, node offsets,
-- optional id, and a list of classes. This is so we can find the target node
-- even if the underlying structure changes slightly.

locate :: forall a t n. (ArrowTree a, ArrowNavigatableTree a, ArrowXml a,
                         Tree t, NavigatableTree t,
                         XN.XmlNode n)
       => Locator -> a (t n) (Int, t n)
locate l =
   (constA 0 &&& Cat.id) >>> (locate' l)
 where
   locate' [] = Cat.id
   locate' ll@(LocatorNode {..}:ls) = -- trcMsg 5 (show ll) >>> trc >>>
       second (findByTag <+> findByOffset <+> findById) <+> findByClass
--       >>> trc
       >>> locate' ls

    where
--      trcMsg a b = (Tr.traceMsg a b)
--      trc = second (withoutNav Tr.traceTree)

      findByTag, findByOffset, findById :: a (t n) (t n)

      findByTag    = childAxis  >>> (deep $ hasName lnTag) -- TODO: use multi here?

      findByOffset = childAxis  >>> nRight lnOffset
       where
         nRight :: (ArrowNavigatableTree a, NavigatableTree nt)
                => Int
                -> a (nt b) (nt b)
         nRight 0 = Cat.id
         nRight n = moveRight >>> nRight (n-1)

      findById     = moveToRoot >>> (deep $ hasMId lnId)
       where
        hasMId mId = filterA
         $ getAttrValue0 "id" &&& constL (maybeToList mId) >>> isA (uncurry (==))

      findByClass :: a (Int, t n) (Int, t n)
      findByClass  =
          second childAxis
          >>> addAT ( deep $ ( classMatch >>> filterA (isA (/=0)) ) &&& Cat.id )
       where
         addAT :: a b (Int, b) -> a (Int, b) (Int, b)
         addAT ta = arr fst &&& (snd ^>> ta) >>> arr (\(i, (j,t)) -> (i + j, t))
         classMatch :: a (t n) Int
         classMatch = getAttrValue0 "class"
                      >>> arr (words >>> S.fromList)
                      >>> arr (S.intersection (S.fromList lnClasses) >>> S.size)

locatorFromId id =
    locator (getAttrValue0 "id" >>> isA (==id))

locator :: (ArrowXml a, ArrowTree a)
        => a XmlTree p -> a XmlTree Locator
locator pred = addNav >>> (deep $ filterA (remNav >>> pred)
                      >>> locator')

locator' :: (ArrowXml a, ArrowTree a)
         => a (NTZipper XNode) Locator
locator' = execSLA [] getLocator
 where
   execSLA :: ArrowList a => s -> SLA s b c -> a b s
   execSLA s f = arr $ fst . (runSLA f s)

   getLocator :: SLA [LocatorNode] (NTZipper XNode) (NTZipper XNode)
   getLocator = ifA isAtRoot Cat.id $
                  Cat.id &&& (locatorNode >>> changeState (\s n -> n:s))
                  >>> arr fst >>> parentAxis >>> getLocator

   locatorNode = getInfo >>> arr (\(tag, (off, (id, cl))) ->
       LocatorNode tag off (if id == "" then Nothing else Just id) (words cl))

   getInfo = getName &&& offset 0 &&& getAttrValue "id" &&& getAttrValue "class"

   offset i = ifA moveLeft (moveLeft >>> offset (i+1)) (constA i)
