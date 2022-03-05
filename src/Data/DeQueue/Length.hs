module Data.DeQueue.Length where

import qualified Data.List as L
import Prelude hiding (splitAt)
import qualified Prelude as P

class Length f where
  length :: f a -> Integer

instance Length [] where
  length = toInteger . P.length

splitAt :: Integer -> [a] -> ([a], [a])
splitAt i xs
  | i <= 0 = ([], xs)
  | i > maxInt =
    let (a, bc) = P.splitAt maxBound xs
        (b, c) = L.genericSplitAt (i - maxInt) bc
     in (a ++ b, c)
  | otherwise = P.splitAt (fromInteger i) xs
  where
    maxInt = toInteger (maxBound :: Int)
