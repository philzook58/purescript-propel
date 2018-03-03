module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Tensor
import Data.ST.Tensor

-- I need lots of tests to see if I'm doing ST right.

{-
arrayOf $ chooseInt 0 4
genPos :: Gen Int
genPos = abs `map` (arbitrary :: Gen Int) `suchThat` (_ > 0)

genListOfPos :: Gen (Array Int)
genListOfPos = vectorOf genPos
-}
main :: forall e. QC e Unit
main = do
  log "You should add some tests."
  -- quickCheck $ forAll genListOfPos \x -> x == (shape $ randn x)

