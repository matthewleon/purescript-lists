module Data.List.Ephemeral.Types where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable)

-- | An ephemeral linked list.
newtype List a = List (Unit -> Step a)

-- | A list is either empty (represented by the `Nil` constructor) or non-empty, in
-- | which case it consists of a head element, and another list (represented by the
-- | `Cons` constructor).
data Step a = Nil | Cons a (List a)

-- | Unwrap an ephemeral list
step :: forall a. List a -> Step a
step (List f) = f unit

-- | The empty list.
-- |
-- | Running time: `O(1)`
nil :: forall a. List a
nil = List \_ -> Nil

-- | Attach an element to the front of a lazy list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> List a -> List a
cons x xs = List \_ -> Cons x xs

-- | An infix alias for `cons`; attaches an element to the front of
-- | a list.
-- |
-- | Running time: `O(1)`
infixr 6 cons as :

derive instance newtypeList :: Newtype (List a) _

instance showList :: Show a => Show (List a) where
  show xs = "fromStrict (" <> go (step xs) <> ")"
    where
    go Nil = "Nil"
    go (Cons x xs') = "(Cons " <> show x <> " " <> go (step xs') <> ")"

instance eqList :: Eq a => Eq (List a) where
  eq = eq1

instance eq1List :: Eq1 List where
  eq1 xs ys = go (step xs) (step ys)
    where
    go Nil Nil = true
    go (Cons x xs') (Cons y ys')
      | x == y = go (step xs') (step ys')
    go _ _ = false

instance ordList :: Ord a => Ord (List a) where
  compare = compare1

instance ord1List :: Ord1 List where
  compare1 xs ys = go (step xs) (step ys)
    where
    go Nil Nil = EQ
    go Nil _   = LT
    go _   Nil = GT
    go (Cons x xs') (Cons y ys') =
      case compare x y of
        EQ -> go (step xs') (step ys')
        other -> other

instance lazyList :: Z.Lazy (List a) where
  defer f = List \_ -> step (f unit)

instance semigroupList :: Semigroup (List a) where
  append xs ys = List (go <$> unwrap xs)
    where
    go Nil = step ys
    go (Cons x xs') = Cons x (xs' <> ys)

instance monoidList :: Monoid (List a) where
  mempty = nil

instance functorList :: Functor List where
  map f xs = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs') = Cons (f x) (f <$> xs')

instance functorWithIndexList :: FunctorWithIndex Int List where
  mapWithIndex f = foldrWithIndex (\i x acc -> f i x : acc) nil

instance foldableList :: Foldable List where
  -- calls foldl on the reversed list
  foldr op z xs = foldl (flip op) z (rev xs) where
    rev = foldl (flip cons) nil

  foldl op = go
    where
    -- `go` is needed to ensure the function is tail-call optimized
    go b xs =
      case step xs of
        Nil -> b
        Cons hd tl -> go (b `op` hd) tl

  foldMap f = foldl (\b a -> b <> f a) mempty

instance foldableWithIndexList :: FoldableWithIndex Int List where
  foldrWithIndex f b xs =
    -- as we climb the reversed list, we decrement the index
    snd $ foldl
            (\(Tuple i b') a -> Tuple (i - 1) (f (i - 1) a b'))
            (Tuple len b)
            revList
    where
    Tuple len revList = rev (Tuple 0 nil) xs
      where
      -- As we create our reversed list, we count elements.
      rev = foldl (\(Tuple i acc) a -> Tuple (i + 1) (a : acc))
  foldlWithIndex f acc =
    snd <<< foldl (\(Tuple i b) a -> Tuple (i + 1) (f i b a)) (Tuple 0 acc)
  foldMapWithIndex f = foldlWithIndex (\i acc -> append acc <<< f i) mempty

instance unfoldableList :: Unfoldable List where
  unfoldr = go where
    go f b = Z.defer \_ -> case f b of
      Nothing -> nil
      Just (Tuple a b') -> a : go f b'

instance traversableList :: Traversable List where
  traverse f =
    foldr (\a b -> cons <$> f a <*> b) (pure nil)

  sequence = traverse id

instance traversableWithIndexList :: TraversableWithIndex Int List where
  traverseWithIndex f =
    foldrWithIndex (\i a b -> cons <$> f i a <*> b) (pure nil)

instance applyList :: Apply List where
  apply = ap

instance applicativeList :: Applicative List where
  pure a = a : nil

instance bindList :: Bind List where
  bind xs f = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs') = step (f x <> bind xs' f)

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = nil

instance alternativeList :: Alternative List

instance monadZeroList :: MonadZero List

instance monadPlusList :: MonadPlus List

instance extendList :: Extend List where
  extend f l =
    case step l of
      Nil -> nil
      Cons a as ->
        f l : (foldr go { val: nil, acc: nil } as).val
        where
        go a { val, acc } =
          let acc' = a : acc
          in { val: f acc' : val, acc: acc' }
