{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeQueue
  ( DeQueue,
    cons,
    fromList,
    length,
    reverse,
    singleton,
    snoc,
    uncons,
    unsnoc,
  )
where

import Data.DeQueue.Core
import Data.DeQueue.LenList (LenList)
import qualified Data.DeQueue.LenList as LL
import Data.DeQueue.Length (length)
import Data.Foldable (toList)
import qualified Data.Foldable
import Data.Functor ((<&>))
import Prelude hiding (length, reverse)

instance Foldable DeQueue where
  foldMap f = foldMap f . toLenList
  length = fromInteger . length
  toList = toList . toLenList

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

fromList :: [a] -> DeQueue a
fromList = fromLenList . LL.fromList

cons :: a -> DeQueue a -> DeQueue a
cons x = prepend (LL.singleton x)

singleton :: a -> DeQueue a
singleton = fromLenList . LL.singleton

snoc :: DeQueue a -> a -> DeQueue a
snoc xs x = xs <> singleton x

uncons :: DeQueue a -> Maybe (a, DeQueue a)
uncons xs = unsnoc (reverse xs) <&> \(ys, y) -> (y, reverse ys)

instance Eq a => Eq (DeQueue a) where
  x == y = toList x == toList y

instance Show a => Show (DeQueue a) where
  showsPrec d xs = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toList xs)
