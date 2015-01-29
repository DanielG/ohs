{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
module OHS.ArrowReader where

import           Prelude hiding (id, (.))

import           Control.Category

import           Control.Arrow
import           Control.Arrow.ArrowIf
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowNF
import           Control.Arrow.ArrowTree
import           Control.Arrow.ArrowNavigatableTree
import           Control.Arrow.ListArrow

class Arrow a => ArrowReader r a where
    askA :: a b r
    localA :: r -> a b c -> a b c

newtype RLA r a b = RLA { runRLA :: r -> LA a b }

instance Category (RLA r) where
    id = RLA $ \_ -> id
    RLA g . RLA f = RLA $ \r -> g r . f r

instance Arrow (RLA r) where
    arr f = RLA $ \_ -> arr f
    first  (RLA f)  = RLA $ \r -> first (f r)
    second (RLA g)  = RLA $ \r -> second (g r)
    RLA f *** RLA g = RLA $ \r -> f r *** g r
    RLA f &&& RLA g = RLA $ \r -> f r &&& g r

instance ArrowZero (RLA r) where
    zeroArrow = RLA $ \_ -> zeroArrow

instance ArrowPlus (RLA r) where
    RLA f <+> RLA g = RLA $ \r -> f r <+> g r

instance ArrowChoice (RLA r) where
    left  (RLA f)   = RLA $ \r -> left (f r)
    right (RLA f)   = RLA $ \r -> right (f r)
    RLA f +++ RLA g = RLA $ \r -> f r +++ g r
    RLA f ||| RLA g = RLA $ \r -> f r ||| g r

instance ArrowApply (RLA r) where
    app = RLA $ \r -> LA $ \(RLA f, b) -> (runLA $ f r) b

instance ArrowList (RLA r) where
    arrL f  = RLA $ \_ -> (arrL f)

    arr2A f = RLA $ \r -> arr2A (flip (runRLA . f) r)

    isA p   = RLA $ \_ -> isA p

    RLA f >>. g = RLA $ \r -> f r >>. g

    withDefault (RLA f) d = RLA $ \r -> withDefault (f r) d

instance ArrowReader r (RLA r) where
    askA = RLA $ \r -> constA r
-- :: r -> a b c -> a b c
    localA r (RLA f) = RLA $ \_ -> f r
