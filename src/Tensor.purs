module Data.Tensor where

import Data.Function.Uncurried
import Prelude ((<<<), class Semiring)
import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.Types

type Shape = Array Int
data DeviceType = GPU | CPU
data DType = UInt8 | Float32 | Bool | Int32 | Error

foreign import data Tensor :: Type -> Type

--foreign import tensor :: forall e. Array Number -> Eff e (Tensor Number)
foreign import tensor :: Array Number -> Tensor Number
foreign import tensor2 :: Array (Array Number) -> Tensor Number
foreign import zeros :: Shape -> Tensor Number
foreign import ones :: Shape -> Tensor Number
foreign import eye :: Int -> Tensor Number
foreign import toString :: forall a. Tensor a -> String
foreign import shape :: forall a. Tensor a -> Shape
foreign import rank :: forall a. Tensor a -> Int
foreign import dtypeImpl :: forall a. Tensor a -> String

foreign import dataSync :: Tensor Number -> Float32Array

foreign import onesLike :: forall a. Tensor a -> Tensor Number
foreign import zerosLike :: forall a. Tensor a -> Tensor Number

foreign import transpose :: forall a. Tensor a -> Tensor a
-- axes keepsdims
foreign import reduceSumImpl :: forall a. Semiring a => Fn3 (Array Int) Boolean (Tensor a) (Tensor a)
reduceSum :: forall a. Semiring a => Array Int -> Boolean -> Tensor a -> Tensor a
reduceSum axes keepdim t = runFn3 reduceSumImpl axes keepdim t
foreign import reduceMeanImpl :: forall a. Semiring a => Fn3 (Array Int) Boolean (Tensor a) (Tensor a)
reduceMean :: forall a. Semiring a => Array Int -> Boolean -> Tensor a -> Tensor a
reduceMean axes keepdim t = runFn3 reduceMeanImpl axes keepdim t
foreign import reduceMaxImpl :: forall a. Semiring a => Fn3 (Array Int) Boolean (Tensor a) (Tensor a)
reduceMax :: forall a. Semiring a => Array Int -> Boolean -> Tensor a -> Tensor a
reduceMax axes keepdim t = runFn3 reduceMaxImpl axes keepdim t


foreign import reduceLogSumExpImpl :: forall a. Semiring a => Fn3 (Array Int) Boolean (Tensor a) (Tensor a)
reduceLogSumExp :: forall a. Semiring a => Array Int -> Boolean -> Tensor a -> Tensor a
reduceLogSumExp axes keepdim t = runFn3 reduceLogSumExpImpl axes keepdim t


{-
instance semiringTensor :: Semiring a => Semiring (Tensor a) where
   add x y = runFn2 addImpl x y
   one = ones
   mul x y = runFn2 addImpl x y
   zero = zeros
   -}

foreign import addImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
add :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
add x y = runFn2 addImpl x y

foreign import mulImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
mul :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
mul x y = runFn2 addImpl x y

foreign import dotImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
dot :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
dot x y = runFn2 dotImpl x y

foreign import reshapeImpl :: forall a. Fn2 Shape (Tensor a) (Tensor a)
reshape :: forall a. Shape -> Tensor a -> Tensor a
reshape shape t = runFn2 reshapeImpl shape t



dtypeStr :: String -> DType
dtypeStr "float32" = Float32
dtypeStr "int32" = Int32
dtypeStr  "uint8" = UInt8
dtypeStr "bool" = Bool
dtypeStr _ = Error

dtype :: forall a. Tensor a -> DType
dtype = dtypeStr <<< dtypeImpl


foreign import fillImpl :: Fn2 Number Shape (Tensor Number)
fill :: Number -> Shape -> Tensor Number
fill num shape = runFn2 fillImpl num shape

foreign import randn :: Shape -> (Tensor Number)
--randn :: Shape -> Tensor Number
--randn shape seed = runFn2 randnImpl shape seed

foreign import linspaceImpl :: Fn3 Number Number Int (Tensor Number) -- start stop Num
linspace :: Number -> Number -> Int -> Tensor Number
linspace start stop num = runFn3 linspaceImpl start stop num

foreign import rangeImpl :: Fn3 Int Int Int (Tensor Int) -- I think. start stop and delta
-- matching ordinary purescript convenction
range :: Int -> Int -> Tensor Int 
range start stop = runFn3 rangeImpl start stop 1

deltarange :: Int -> Int -> Int -> Tensor Int 
deltarange start stop delta = runFn3 rangeImpl start stop delta



