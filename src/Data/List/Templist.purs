module Data.List.Templist where

import Prelude
import Control.Lazy as Z
import Control.Monad.Rec.Class as MR
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (class Foldable, foldr)
import Data.Unfoldable (class Unfoldable)

newtype List a = List (Unit -> Step a)
data Step a = Nil | Cons a (List a)

derive instance newtypeList :: Newtype (List a) _

instance lazyList :: Z.Lazy (List a) where
  defer f = List \_ -> step (f unit)

instance functorList :: Functor List where
  map f xs = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs') = Cons (f x) (f <$> xs')

instance unfoldableList :: Unfoldable List where
  unfoldr = go where
    go f b = List \_ -> case f b of
      Nothing -> Nil
      Just (Tuple a b') -> Cons a (go f b')

step :: List ~> Step
step l = unwrap l $ unit

nil :: forall a. List a
nil = List \_ -> Nil

cons :: forall a. a -> List a -> List a
cons x xs = List \_ -> Cons x xs
infixr 6 cons as :

fromFoldable :: forall f. Foldable f => f ~> List
fromFoldable = foldr cons nil

traverseR_ :: forall m a b. MR.MonadRec m => (a -> m b) -> List a -> m Unit
traverseR_ f = MR.tailRecM go
  where
    go xs = case step xs of
      Nil -> pure (MR.Done unit)
      (Cons x xs') -> f x *> pure (MR.Loop xs')

sequenceR_ :: forall m t. MR.MonadRec m => List (m t) -> m Unit
sequenceR_ = traverseR_ id
