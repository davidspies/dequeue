{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DeQueue.LenList.Core (LenList, reverse, splitAt, uncons) where

import Data.DeQueue.Length (Length (..))
import qualified Data.DeQueue.Length as L
import qualified Data.Foldable
import GHC.Exts (IsList (..))
import Prelude hiding (length, reverse, splitAt)
import qualified Prelude as P

data LenList a = LenList Integer [a]
  deriving (Functor, Traversable)

instance Foldable LenList where
  foldMap f = foldMap f . toList
  toList = toList
  null xs = length xs == 0
  length = fromInteger . length

instance IsList (LenList a) where
  type Item (LenList a) = a
  fromList xs = LenList (length xs) xs
  toList (LenList _ xs) = xs

instance Length LenList where
  length (LenList n _) = n

instance Semigroup (LenList a) where
  LenList nx x <> LenList ny y = LenList (nx + ny) (x <> y)

uncons :: LenList a -> Maybe (a, LenList a)
uncons (LenList n xs) = case xs of
  [] -> Nothing
  y : ys -> Just (y, LenList (n - 1) ys)

splitAt :: Integer -> LenList a -> (LenList a, LenList a)
splitAt i (LenList n xs) = (LenList beforeLen before, LenList (n - beforeLen) after)
  where
    (before, after) = L.splitAt i xs
    beforeLen = min i n

reverse :: LenList a -> LenList a
reverse (LenList n xs) = LenList n (P.reverse xs)
