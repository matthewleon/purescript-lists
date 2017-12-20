module Bench.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.List (range, reverse, reverse')
import Performance.Minibench (bench, benchWith)
import Test.Assert (ASSERT, assert)

main :: Eff (assert :: ASSERT, console :: CONSOLE) Unit
main = do
  let longList = range 0 100000
  bench \_ -> reverse' longList
  bench \_ -> reverse longList
  {-
  log "test reverse equivalence"
  assert $ reverse longList == reverse' longList
  -}
