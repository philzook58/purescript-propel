module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tensor
import Data.Tensor.Eff


main :: forall e. Eff (console :: CONSOLE  | e) Unit
main = do
  log $ toString $ zeros [2,2] :: Tensor Int
  log $ toString $ ones [2,2] :: Tensor Number
  log $ toString $ fill 3.2 [2,3] 
  log $ toString $ eye 7
  log $ toString $ range 1 9
  log $ toString $ deltarange 1 7 2
  log $ toString $ linspace 1.343 0.32432 7
  log $ toString $ randn [3,2]
  log $ show $ shape $ randn [2,3] :: Tensor Number
  log $ toString $ tensor [2.0,2.0,4.0,3.0]
  log $ toString $ tensor [2,2,4,3]
  log $ toString $ tensor2 [[2.0,2.0],[4.0,3.0]]
  log $ toString $ reshape [3,2] $ fill 3.2 [2,3] 
  let y = fill 3.2 [2,3] 
  let z = fill 3.2 [2,3]
  q <- pure $ addT y z 
  log $ toString $ addT y z
  log $ toString $ mulT y z
  log $ toString $ divT y z
  log $ toString $ y
  log $ toString $ z
  log $ toString $ dot (tensor2 [[2.0,0.0],[0.0,2.0]] ) (tensor2 [[4.0,0.0],[0.0,6.0]] )
  log $ toString $ reduceSum [0] true (eye 3)
  log $ toString $ reduceSum [1] true (eye 3)
  log $ toString $ argmin 0 (eye 3)
  log $ toString $ divT (eye 3) (fill 2.0 [3,3])
  log $ toString $ equal (eye 3) (eye 3) 
  log $ toString $ (ones [1,4] :: Tensor Number )
  log $ toString $ fill 1.0 [1,2]
  log $ toString $ tensor [true, false]
  log $ show $ asArray $ tensor [true, false]
  log $ toString $ zerosLike $ eye 7
  log $ toString $ tensor2 [[1,3],[4,5]]
  log $ show $ rank y
  --dispose y
  --log $ toString y -- Produces error. y is nulled out by dispose


