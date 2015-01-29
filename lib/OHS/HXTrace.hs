{-# LANGUAGE FlexibleContexts #-}
module OHS.HXTrace
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO
import Control.Arrow.ArrowNavigatableTree

import Data.Tree.NavigatableTree.Class
import Data.Tree.NTree.TypeDefs

import System.IO                        ( hPutStrLn
                                        , hFlush
                                        , stderr
                                        )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlState.SystemConfig

import Text.XML.HXT.Arrow.Edit          ( addHeadlineToXmlDoc
                                        , treeRepOfXmlDoc
                                        , indentDoc
                                        )
import qualified Debug.Trace

-- ------------------------------------------------------------


-- | apply a trace arrow and issue message to stderr

trace                   :: ArrowList a
                        => a b String -> a b b
trace trc               = (this &&& trc) >>> arr (\(b, str) -> Debug.Trace.trace str b)

traceValue              :: ArrowList a => (b -> String) -> a b b
traceValue trc          = trace (arr $ (('-' : "- ") ++) . trc)


traceMsg                :: ArrowList a => String -> a b b
traceMsg       msg      = traceValue (const msg)

traceSource             :: ArrowXml a => a XmlTree XmlTree
traceSource             = trace $
                          xshow $
                          choiceA [ isRoot :-> ( indentDoc
                                                 >>>
                                                 getChildren
                                               )
                                  , isElem :-> ( root [] [this]
                                                 >>> indentDoc
                                                 >>> getChildren
                                                 >>> isElem
                                               )
                                  , this   :-> this
                                  ]

-- | issue the tree representation of a document if trace level >= 4
traceTree               :: (ArrowXml a, ToXmlTree t XNode, FromXmlTree t XNode) => a (t XNode) (t XNode)
traceTree               = arr toXmlTree >>>
                          (
                            trace $
                            xshow $
                            treeRepOfXmlDoc
                            >>>
                            addHeadlineToXmlDoc
                            >>>
                            getChildren
                          ) >>> arr fromXmlTree


-- | trace a main computation step
-- issue a message when trace level >= 1, issue document source if level >= 3, issue tree when level is >= 4

traceDoc                :: ArrowXml a => String -> a XmlTree XmlTree
traceDoc msg            = traceMsg msg
                          >>>
                          traceSource
                          >>>
                          traceTree
