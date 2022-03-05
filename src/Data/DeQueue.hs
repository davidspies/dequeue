{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeQueue (DeQueue, cons, length, reverse, snoc, uncons, unsnoc) where

import Data.DeQueue.Core
import Data.DeQueue.LenList (LenList)
import Data.DeQueue.Length (length)
import qualified Data.Foldable
import Data.Functor ((<&>))
import GHC.Exts (IsList (..))
import Prelude hiding (length, reverse)

instance Foldable DeQueue where
  foldMap f = foldMap f . toLenList
  length = fromInteger . length
  toList = toList

instance Semigroup (DeQueue a) where
  x <> y
    | length x <= length y = prepend (toLenList x) y
    | otherwise = reverse $ prepend (toLenList $ reverse y) (reverse x)

instance Monoid (DeQueue a) where
  mempty = empty

fromLenList :: LenList a -> DeQueue a
fromLenList = (`prepend` mempty)

instance Functor DeQueue where
  fmap f = fromLenList . fmap f . toLenList

instance Traversable DeQueue where
  traverse f = fmap fromLenList . traverse f . toLenList

instance IsList (DeQueue a) where
  type Item (DeQueue a) = a
  fromList = fromLenList . fromList
  toList = toList . toLenList

cons :: a -> DeQueue a -> DeQueue a
cons x = prepend [x]

snoc :: DeQueue a -> a -> DeQueue a
snoc xs x = xs <> [x]

uncons :: DeQueue a -> Maybe (a, DeQueue a)
uncons xs = unsnoc (reverse xs) <&> \(ys, y) -> (y, reverse ys)

instance Eq a => Eq (DeQueue a) where
  x == y = toList x == toList y

instance Show a => Show (DeQueue a) where
  showsPrec d xs = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toList xs)
