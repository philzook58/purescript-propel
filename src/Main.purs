module Main where

import Prelude hiding (add)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Unsafe.Coerce
import Data.Tensor
import Data.ST.Tensor
import Control.Monad.ST
--foreign import data Shape :: Type

{-

-- No not Free. That would allow different array depth in each entry... Is that right? No. Free is what I want
-- 
foreign import tensor :: Array Number -> Tensor Number
tensorfree :: Free Array Number -> Tensor Number
tensor2 :: Array (Array Number) -> Tensor Number

foreign import shape :: forall a. Tensor a -> Shape
foreign import dtypeStr :: forall a. Tensor a -> String
data DType = UInt8 | Float32 | Bool | Int32 
dtype :: forall a. Tensor a -> DType

-- assignment destroys its argument. I am not sure what this is for? I am not sure how to enforce this destruction. Is the lack of ST h' sufficent in the result? Remove r?
-- foreign import assign :: forall h h' r. STRef h (Tensor Number) -> STRef h' (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

foreign import add :: forall h r. STRef h (Tensor Number) -> Tensor Number -> Eff (st :: ST h | r) (Tensor Number)
-- Ok, we also need to check for shape mismatch. Thse things might call error :: ERROR effect?
add x y = runST $ (add x) =>= (copy y)

foreign import square :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import neg :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import exp :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import log :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import sinh :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import square :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

foreign import setDiag :: forall h r. STRef h (Tensor Number) -> (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

foreign import reshape :: forall h r. Shape -> STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import flatten :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import slice :: forall h r. Shape -> Shape -> STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number) -- Should I call these types Shape?

-- or flatten = reshape [-1]
-- which is it's implementation
-- This has a relatively non evil form


foreign import reduceSum :: (Tensor Number) -> (Tensor Number)
foreign import reduceMean :: (Tensor Number) -> (Tensor Number)
foreign import reduceMax :: (Tensor Number) -> (Tensor Number)

foreign import eq :: (Tensor Number) -> (Tensor Number) -> (Tensor Number)

foreign import copy :: forall h r. (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

t (!!) n = (_ !! n) <<< getData

class BasicTensor a where -- BasicTensor f a? where a would be FLoat32 for example
   shape :: Shape
   dtype :: DType
   getData :: Float32Array
   dispose :: forall h r. STRef h a -> Eff (st :: ST h | r) Unit

instance basictensorTensorNumber :: BasicTensor (Tensor Number) Float32 where

class IsDType a

instance IsDType Float32

-- garbage collection...?
-- maybe GarbageT
-- or some garbage Monad
-- destroy reference is running the ST monad
-- gc :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) Unit

-}
{-
instance eqTensor :: Eq (Tensor Number) where
   eq = runST 
              copy 
             <- eqST 
             reduceAnd -- if this is a thing? 
-}

{-

-- one = eye(n)

foriegn data Tensor


instance Semiring Tensor where

pure = fill


tensor :: Array Number -> Tensor
tensor :: Free Array Number -> Tensor







module Data.Propel.ST.Tensor where



-}
--- Also something to be said for FKron a b = [Tuple a b]
-- tkron :: Tensor -> Tensor -> Tensor

-- typed  does reshape in matrix form, matmul, then unreshape

-- matmul will have to be in the Maybe monad? Or should I be more unopinionated than that
-- Unsafe module


-- Can Form Category of Tensor objects?

-- Make Tensor a functor. No. Make Tensor :: * -> *
-- but only give constructors for (Tensor Number) variant. That's a good start.
-- actually only "float32" | "int32" | "uint8" | "bool"

-- hmm. But map will be unsafe. So not a Functor.
-- I can't actually support map anyhow


-- getDataF32 :: Tensor Number -> TypedFloat32Array
squareexample :: Tensor Number
squareexample = pureST do
                            y <- unsafeThaw (fill 3.2 [2,2])
                            square y
                            freeze y
{-
reshapeexample :: Tensor Number
reshapeexample = pureST do 
                            y <- unsafeThaw (fill 3.2 [4,4])
                            reshape [2,2,2,2] y
                            freeze y
-}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

  log $ toString $ zeros [2,2]
  log $ toString $ ones [2,2]
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
  log $ toString $ squareexample
  --log $ toString $ reshapeexample
  log $ toString $ reshape [3,2] $ fill 3.2 [2,3] 
  let y = fill 3.2 [2,3] 
  let z = fill 3.2 [2,3]
  q <- pure $ add y z 
  log $ toString $ add y z
  log $ toString $ y
  log $ toString $ z
  log $ toString $ dot (tensor2 [[2.0,0.0],[0.0,2.0]] ) (tensor2 [[4.0,0.0],[0.0,6.0]] )
  log $ toString $ reduceSum [0] true (eye 3)
  log $ toString $ reduceSum [1] true (eye 3)
  --log $ toString t
  


