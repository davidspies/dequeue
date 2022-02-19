{-# LANGUAGE DeriveTraversable #-}

module Data.DeQueue.LenList.Core (LenList, fromList, reverse, splitAt, uncons) where

import Data.DeQueue.Length (Length (..))
import qualified Data.DeQueue.Length as L
import Data.Foldable (toList)
import Prelude hiding (length, reverse, splitAt)
import qualified Prelude as P

data LenList a = LenList Integer [a]
  deriving (Functor, Traversable)

instance Foldable LenList where
  foldMap f = foldMap f . toList
  length = fromInteger . length
  toList (LenList _ xs) = xs

instance Length LenList where
  length (LenList n _) = n

{-@ type CorrectLenList a = {xs : LenList a | length xs == length (toList xs)} @-}

instance Semigroup (LenList a) where
  {-@ (<>) :: CorrectLenList a -> CorrectLenList a -> CorrectLenList a @-} 
  LenList nx x <> LenList ny y = LenList (nx + ny) (x <> y)

{-@ uncons :: CorrectLenList a -> Maybe (a, CorrectLenList a) @-}
uncons :: LenList a -> Maybe (a, LenList a)
uncons (LenList n xs) = case xs of
  [] -> Nothing
  y : ys -> Just (y, LenList (n - 1) ys)

fromList :: [a] -> LenList a
fromList xs = LenList (length xs) xs

splitAt :: Integer -> LenList a -> (LenList a, LenList a)
splitAt i (LenList n xs) = (LenList beforeLen before, LenList (n - beforeLen) after)
  where
    (before, after) = L.splitAt i xs
    beforeLen = min i n

reverse :: LenList a -> LenList a
reverse (LenList n xs) = LenList n (P.reverse xs)
