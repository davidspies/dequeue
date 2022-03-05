{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeQueue.LenList (module X) where

import Data.DeQueue.LenList.Core as X

instance Monoid (LenList a) where
  mempty = []
