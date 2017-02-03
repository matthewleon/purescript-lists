module Data.List.Increasing where

import Prelude
import Data.List (List(..), (:))
import Data.List as L
import Data.Newtype (class Newtype)
import Data.Generic (class Generic)

-- do not export this constructor
newtype IncreasingList a = UnsafeIncreasingList (List a)
derive newtype instance showIncreasingList :: Show IncreasingList a
derive newtype instance eqIncreasingList :: Eq IncreasingList
derive newtype instance genericIncreasingList :: Generic IncreasingList

toList :: IncreasingList ~> List
toList (UnsafeIncreasingList xs) = xs

--------------------------------------------------------------------------------
-- List creation ---------------------------------------------------------------
--------------------------------------------------------------------------------
singleton :: forall a. a -> IncreasingList a
singleton = UnsafeIncreasingList <<< L.singleton

sort :: forall a. Ord a => List a -> IncreasingList a
sort xs = UnsafeIncreasingList <<< mergeAll <<< sequences
  where
  sequences :: List a -> List (List a)
  sequences (a : b : xs)
    | a `compare` b == GT = descending b (L.singleton a) xs
    | otherwise = ascending b (a : _) xs
  sequences xs = L.singleton xs

  descending :: a -> List a -> List a -> List (List a)
  descending a as (b : bs)
    | a `compare` b == GT = descending b (a : as) bs
  descending a as bs = (a : as) : sequences bs

  ascending :: a -> (List a -> List a) -> List a -> List (List a)
  ascending a as (b : bs)
    | a `compare` b /= GT = ascending b (\ys -> as (a : ys)) bs
  ascending a as bs = ((as $ L.singleton a) : sequences bs)

  mergeAll :: List (List a) -> List a
  mergeAll (x : Nil) = x
  mergeAll xs = mergeAll (mergePairs xs)

  mergePairs :: List (List a) -> List (List a)
  mergePairs (a : b : xs) = merge a b : mergePairs xs
  mergePairs xs = xs

  merge :: List a -> List a -> List a
  merge as@(a : as') bs@(b : bs')
    | a `compare` b == GT = b : merge as bs'
    | otherwise       = a : merge as' bs
  merge Nil bs = bs
  merge as Nil = as

--------------------------------------------------------------------------------
-- List size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. IncreasingList a -> Boolean
null (UnsafeIncreasingList xs) = null xs

-- | Get the length of a list
-- |
-- | Running time: `O(n)`
length :: forall a. IncreasingList a -> Int
length (UnsafeIncreasingList xs) = L.length xs

--------------------------------------------------------------------------------
-- Extending lists -------------------------------------------------------------
--------------------------------------------------------------------------------

--TODO: snoc
-- | Insert an element into an increasing list.
-- |
-- | Running time: `O(n)`
insert :: forall a. Ord a => a -> IncreasingList a -> IncreasingList a
insert x Nil = singleton x
insert x ys@(y : ys') =
  case compare x y of
    GT -> y : (insert x ys')
    _  -> x : ys

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x
