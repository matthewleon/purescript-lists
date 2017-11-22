module Data.List.ST where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.List.Types (List)
import Unsafe.Coerce (unsafeCoerce)

data STList h a = Nil | Cons a (STList h a)

setHead :: forall a r h. STList h a -> Eff (st :: ST h | r) Unit

unsafeFreeze :: forall a r h. STList h a -> Eff (st :: ST h | r) (List a)
unsafeFreeze = pure <<< unsafeCoerce
