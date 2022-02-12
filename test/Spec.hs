{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.DeQueue
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Test.QuickCheck
import Prelude hiding (length, reverse)
import qualified Prelude as P

-- By building an arbitrary DeQueue by concatenating two separate DeQueues of arbitrary lengths,
-- we can cover all possible internal states (unlike when we just call `fromList` and get a
-- perfectly even split).
data ConstructDeQueue a = ConstructDeQueue [a] [a]
  deriving (Eq, Ord, Show, Generic)

constructQ :: ConstructDeQueue a -> DeQueue a
constructQ (ConstructDeQueue xs ys) = fromList xs <> fromList ys

constructL :: ConstructDeQueue a -> [a]
constructL (ConstructDeQueue xs ys) = xs ++ ys

instance Arbitrary a => Arbitrary (ConstructDeQueue a) where
  arbitrary = do
    largeList <- arbitrary
    -- Keep the smaller list small enough to usually avoid toppling. This gives us more interesting
    -- splits.
    smallLen <- choose (0, (F.length largeList + 3) `quot` 2)
    smallList <- vector smallLen
    elements [ConstructDeQueue largeList smallList, ConstructDeQueue smallList largeList]
  shrink = genericShrink

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy

prop_mempty_is_empty :: (Eq a, Show a) => Proxy a -> Property
prop_mempty_is_empty (Proxy :: Proxy a) = toList (mempty :: DeQueue a) === []

prop_appendLikeList :: (Eq a, Show a) => ConstructDeQueue a -> ConstructDeQueue a -> Property
prop_appendLikeList x y =
  toList (constructQ x <> constructQ y) === constructL x ++ constructL y

prop_consLikeList :: (Eq a, Show a) => a -> ConstructDeQueue a -> Property
prop_consLikeList x y = toList (cons x (constructQ y)) === x : constructL y

prop_snocLikeList :: (Eq a, Show a) => ConstructDeQueue a -> a -> Property
prop_snocLikeList x y = toList (snoc (constructQ x) y) === constructL x ++ [y]

prop_toListReturnsList :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_toListReturnsList xs = toList (constructQ xs) === constructL xs

prop_correctLength :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_correctLength xs = length (constructQ xs) === length (constructL xs)

prop_reverseLikeList :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_reverseLikeList xs = toList (reverse (constructQ xs)) === P.reverse (constructL xs)

prop_singletonLikeList :: (Eq a, Show a) => a -> Property
prop_singletonLikeList x = toList (singleton x) === [x]

prop_unconsLikeList :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_unconsLikeList xs = case uncons (constructQ xs) of
  Nothing -> constructL xs === []
  Just (y, ys) -> constructL xs === y : toList ys

prop_unsnocLikeList :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_unsnocLikeList xs = case unsnoc (constructQ xs) of
  Nothing -> constructL xs === []
  Just (ys, y) -> constructL xs === toList ys ++ [y]

prop_fromListToListRoundTrip :: (Eq a, Show a) => [a] -> Property
prop_fromListToListRoundTrip xs = toList (fromList xs) === xs

prop_toListFromListRoundTrip :: (Eq a, Show a) => ConstructDeQueue a -> Property
prop_toListFromListRoundTrip xs = fromList (toList (constructQ xs)) === constructQ xs

return []

main :: IO ()
main = do
  True <- $quickCheckAll
  return ()
