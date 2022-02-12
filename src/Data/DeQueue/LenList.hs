{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeQueue.LenList (singleton, module X) where

import Data.DeQueue.LenList.Core as X

instance Monoid (LenList a) where
  mempty = fromList []

singleton :: a -> LenList a
singleton x = fromList [x]
