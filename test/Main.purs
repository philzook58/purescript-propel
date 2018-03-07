module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Tensor


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
  -- quickCheck $ forAll genListOfPos \x -> x == (shape $ randn x)
  log $ toString $ zeros [2,2]
  log $ toString $ ones [2,2] :: Tensor Number
  log $ toString $ fill 3.2 [2,3] 
  log $ toString $ eye 7
  log $ toString $ range 1 9
  log $ toString $ deltarange 1 7 2
  log $ toString $ linspace 1.343 0.32432 7
  log $ toString $ randn [3,2]
  --_ <- pure $ tensor [2.0,2.0,4.0,3.0]
  log $ show $ shape $ randn [2,3]
  log $ toString $ tensor [2.0,2.0,4.0,3.0]
  log $ toString $ tensor2 [[2.0,2.0],[4.0,3.0]]
  --log $ toString $ squareexample
  --log $ toString $ reshapeexample
  log $ toString $ reshape [3,2] $ fill 3.2 [2,3] 
  let y = fill 3.2 [2,3] 
  let z = fill 3.2 [2,3]
  q <- pure $ addT y z 
  log $ toString $ addT y z
  log $ toString $ y
  log $ toString $ z
  log $ toString $ dot (tensor2 [[2.0,0.0],[0.0,2.0]] ) (tensor2 [[4.0,0.0],[0.0,6.0]] )
  log $ toString $ reduceSum [0] true (eye 3)
  log $ toString $ reduceSum [1] true (eye 3)
  log $ toString $ argmin 0 (eye 3)
  log $ toString $ divT (eye 3) (fill 2.0 [3,3])
  log $ toString $ equal (eye 3) (eye 3) 
  log $ toString $ (ones [1,4] :: Tensor Number )

