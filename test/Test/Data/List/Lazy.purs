module Test.Data.List.Lazy (testListLazy) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.List.Lazy (replicate, length)
import Test.Assert (ASSERT)

testListLazy :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testListLazy = logShow (length (replicate 1000000 unit))
