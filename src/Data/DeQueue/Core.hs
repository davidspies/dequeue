{-# LANGUAGE NamedFieldPuns #-}

module Data.DeQueue.Core (DeQueue, empty, prepend, reverse, toLenList, unsnoc) where

import Data.DeQueue.LenList (LenList)
import qualified Data.DeQueue.LenList as LL
import Data.DeQueue.Length (Length (..))
import Prelude hiding (length, reverse, splitAt)

data DeQueue a = DeQueue {front :: LenList a, back :: LenList a}

{-@ balanced :: DeQueue a -> Bool @-}
balanced :: DeQueue a -> Bool
balanced DeQueue {front, back} =
  length front <= 2 * length back + 1 && length back <= 2 * length front + 1

{-@ type Balanced a = { q : DeQueue a | length (front q) <= 2 * length (back q) + 1 && length (back q) <= 2 * length (front q) + 1 } @-}

{-@ reverse :: Balanced a -> Balanced a @-}
reverse :: DeQueue a -> DeQueue a
reverse DeQueue {front, back} = DeQueue {front = back, back = front}

toLenList :: DeQueue a -> LenList a
toLenList DeQueue {front, back} = front <> LL.reverse back

empty :: DeQueue a
empty = DeQueue mempty mempty

instance Length DeQueue where
  length = length . toLenList

tryTopple :: DeQueue a -> DeQueue a
tryTopple q@DeQueue {front, back}
  | length front > 2 * length back + 1 = DeQueue {front = before, back = back <> LL.reverse after}
  | otherwise = q
  where
    (before, after) = LL.splitAt (length q `quot` 2) front

prepend :: LenList a -> DeQueue a -> DeQueue a
prepend x (DeQueue yfront yback) = tryTopple $ DeQueue (x <> yfront) yback

unsnoc :: DeQueue a -> Maybe (DeQueue a, a)
unsnoc DeQueue {front, back} = case LL.uncons back of
  Nothing -> case LL.uncons front of
    Nothing -> Nothing
    Just (x, _empty) -> Just (empty, x)
  Just (x, xs) -> Just (tryTopple DeQueue {front, back = xs}, x)
