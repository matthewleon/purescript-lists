module Test.Data.List.Lazy (testListLazy) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List.Lazy (repeat)
import Data.Traversable (traverse_)

testListLazy :: forall eff. Eff (console :: CONSOLE | eff) Unit
testListLazy = traverse_ log (repeat "yes")
