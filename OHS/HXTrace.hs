module OHS.HXTrace
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

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

-- ------------------------------------------------------------


-- | apply a trace arrow and issue message to stderr

trace                   :: (ArrowList a, ArrowIO a)
                        => Int -> a b String -> a b b
trace level trc         = perform ( trc
                                    >>>
                                    arrIO (\ msg -> putStrLn msg)
                                  )

traceValue              :: (ArrowList a, ArrowIO a) => Int -> (b -> String) -> a b b
traceValue level trc    = trace level (arr $ (('-' : "- (" ++ show level ++ ") ") ++) . trc)


traceMsg                :: (ArrowList a, ArrowIO a) => Int -> String -> a b b
traceMsg level msg      = traceValue level (const msg)

traceSource             :: (ArrowXml a, ArrowIO a) => a XmlTree XmlTree
traceSource             = trace 3 $
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
traceTree               :: (ArrowXml a, ArrowIO a) => a XmlTree XmlTree
traceTree               = trace 4 $
                          xshow $
                          treeRepOfXmlDoc
                          >>>
                          addHeadlineToXmlDoc
                          >>>
                          getChildren

-- | trace a main computation step
-- issue a message when trace level >= 1, issue document source if level >= 3, issue tree when level is >= 4

traceDoc                :: (ArrowXml a, ArrowIO a) => String -> a XmlTree XmlTree
traceDoc msg            = traceMsg 1 msg
                          >>>
                          traceSource
                          >>>
                          traceTree

-- ----------------------------------------------------------

traceOutputToStderr     :: Int -> String -> IO ()
traceOutputToStderr _level msg
                        = do
                          hPutStrLn stderr msg
                          hFlush stderr

-- ----------------------------------------------------------
