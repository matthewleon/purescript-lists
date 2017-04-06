module Test.Data.List.Lazy (testListLazy) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List.Lazy (length, range)
import Test.Assert (ASSERT, assert)

testListLazy :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testListLazy = do
  log "length should be stack-safe"
  assert $ length (range 1 1000000) == 1000000
