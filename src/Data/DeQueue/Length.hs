module Data.DeQueue.Length where

import Prelude hiding (splitAt)
import qualified Prelude as P

class Length f where
  length :: f a -> Integer

instance Length [] where
  length = toInteger . P.length

splitAt :: Integer -> [a] -> ([a], [a])
splitAt i xs
  | i <= 0 = ([], xs)
  | i > toInteger (maxBound :: Int) =
    let (before, after) = P.splitAt maxBound xs
        rest = if null after then [] else error "Practically unreachable"
     in (before ++ rest, drop (P.length rest) after)
  | otherwise = P.splitAt (fromInteger i) xs
