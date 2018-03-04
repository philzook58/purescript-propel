module Data.Tensor 
   ( Tensor
   , DType(..)
   , Shape
   , DeviceType(..)

   , eye
   , fill	
   , ones
   , randn
   , range
   , deltarange
   , linspace

   , tensor
   , tensor2
   , zeros

   , abs
   , addT
   , argmin
   , argmax
   , cast
   , concat
   , cosh
   , dataSync
   , divT
   , dot
   , dtype
   , equal
   , exp
   , flatten
   , greater
   , greaterEqual
   , less
   , lessEqual
   , log
   , mulT
   , neg
   , onesLike

   , relu
   , reverse
   , reduceSum
   , reduceMax
   , reduceMean
   , reshape
   , shape
   , sinh
   , square
   , toString
   , transpose
   , zerosLike

   ) where

import Data.ArrayBuffer.Types (Float32Array)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Prelude ((<<<), class Semiring, class Ring, class Show, class EuclideanRing, show)

type Shape = Array Int
data DeviceType = GPU | CPU
data DType = UInt8 | Float32 | Bool | Int32 | Error


instance showDType :: Show DType where
	show Float32 = "float32" 
	show Int32 = "int32"
	show  UInt8 =  "uint8"
	show Bool ="bool"
	show Error = "ERROR. DType not found."

foreign import data Tensor :: Type -> Type

--foreign import tensor :: forall e. Array Number -> Eff e (Tensor Number)
foreign import eye :: Int -> Tensor Number

foreign import fillImpl :: Fn2 Number Shape (Tensor Number)
fill :: Number -> Shape -> Tensor Number
fill num shape' = runFn2 fillImpl num shape'

foreign import ones :: Shape -> Tensor Number
foreign import tensor :: Array Number -> Tensor Number
foreign import tensor2 :: Array (Array Number) -> Tensor Number
foreign import zeros :: Shape -> Tensor Number
foreign import randn :: Shape -> (Tensor Number)

foreign import rangeImpl :: Fn3 Int Int Int (Tensor Int) -- I think. start stop and delta
-- matching ordinary purescript convenction
range :: Int -> Int -> Tensor Int 
range start stop = runFn3 rangeImpl start stop 1

deltarange :: Int -> Int -> Int -> Tensor Int 
deltarange start stop delta = runFn3 rangeImpl start stop delta

foreign import linspaceImpl :: Fn3 Number Number Int (Tensor Number) -- start stop Num
linspace :: Number -> Number -> Int -> Tensor Number
linspace start stop num = runFn3 linspaceImpl start stop num

foreign import abs :: Tensor Number -> Tensor Number

foreign import addImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
addT :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
addT x y = runFn2 addImpl x y

foreign import argminImpl :: forall a. Fn2 Int (Tensor a) (Tensor a)
argmin :: forall a. Int -> Tensor a -> Tensor a
argmin axis t = runFn2 argminImpl axis t

foreign import argmaxImpl :: forall a. Fn2 Int (Tensor a) (Tensor a)
argmax :: forall a. Int -> Tensor a -> Tensor a
argmax axis t = runFn2 argmaxImpl axis t

foreign import castImpl :: forall a b. Fn2 String (Tensor a) (Tensor b)
cast :: forall a b. DType -> Tensor a -> Tensor b
cast dtype' t = runFn2 castImpl (show dtype') t 

foreign import concatImpl :: forall a. Fn3 Int (Tensor a) (Tensor a) (Tensor a)
concat :: forall a. Int -> Tensor a -> Tensor a -> Tensor a
concat axis t1 t2 = runFn3 concatImpl axis t1 t2 

foreign import cosh :: Tensor Number -> Tensor Number

-- https://github.com/jutaro/purescript-typedarray
foreign import dataSync :: Tensor Number -> Float32Array

foreign import divImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
divT :: forall a. (EuclideanRing a) => Tensor a -> Tensor a -> Tensor a
divT x y = runFn2 divImpl x y

foreign import dtypeImpl :: forall a. Tensor a -> String
dtypeStr :: String -> DType
dtypeStr "float32" = Float32
dtypeStr "int32" = Int32
dtypeStr  "uint8" = UInt8
dtypeStr "bool" = Bool
dtypeStr _ = Error
dtype :: forall a. Tensor a -> DType
dtype = dtypeStr <<< dtypeImpl



foreign import equalImpl :: forall a. Fn2 (Tensor a) (Tensor a) (Tensor Boolean)
equal :: forall a. Tensor a -> Tensor a -> Tensor Boolean
equal x y = runFn2 equalImpl x y

foreign import exp :: Tensor Number -> Tensor Number
foreign import flatten :: forall a. Tensor a -> Tensor a

foreign import greaterImpl :: forall a. Fn2 (Tensor a) (Tensor a) (Tensor Boolean)
greater :: forall a. Tensor a -> Tensor a -> Tensor Boolean
greater x y = runFn2 greaterImpl x y

foreign import greaterEqualImpl :: forall a. Fn2 (Tensor a) (Tensor a) (Tensor Boolean)
greaterEqual :: forall a. Tensor a -> Tensor a -> Tensor Boolean
greaterEqual x y = runFn2 greaterEqualImpl x y

foreign import lessImpl :: forall a. Fn2 (Tensor a) (Tensor a) (Tensor Boolean)
less :: forall a. Tensor a -> Tensor a -> Tensor Boolean
less x y = runFn2 lessImpl x y

foreign import lessEqualImpl :: forall a. Fn2 (Tensor a) (Tensor a) (Tensor Boolean)
lessEqual :: forall a. Tensor a -> Tensor a -> Tensor Boolean
lessEqual x y = runFn2 lessEqualImpl x y

foreign import log :: Tensor Number -> Tensor Number

foreign import logSoftMax :: Tensor Number -> Tensor Number

foreign import mulImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
mulT :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
mulT x y = runFn2 mulImpl x y

foreign import neg :: forall a. (Ring a) => Tensor a -> Tensor a


foreign import onesLike :: forall a. Tensor a -> Tensor Number

foreign import sinh :: Tensor Number -> Tensor Number

foreign import sigmoid :: Tensor Number -> Tensor Number


foreign import sign :: Tensor Number -> Tensor Number

foreign import square :: forall a. (Semiring a) => Tensor a -> Tensor a -- (Tensor Number)

foreign import squeeze :: forall a. Tensor a -> Tensor a

foreign import tanh :: Tensor Number -> Tensor Number


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



foreign import relu :: Tensor Number -> Tensor Number

foreign import reshapeImpl :: forall a. Fn2 Shape (Tensor a) (Tensor a)
reshape :: forall a. Shape -> Tensor a -> Tensor a
reshape shape' t = runFn2 reshapeImpl shape' t
foreign import reverseImpl :: forall a. Fn2 (Array Int) (Tensor a) (Tensor a)
reverse :: forall a. Array Int -> Tensor a -> Tensor a
reverse dims t = runFn2 reverseImpl dims t


foreign import dotImpl :: forall a. (Semiring a) => Fn2 (Tensor a) (Tensor a) (Tensor a)
dot :: forall a. (Semiring a) => Tensor a -> Tensor a -> Tensor a
dot x y = runFn2 dotImpl x y



foreign import sliceImpl :: forall a. Fn3 (Array Int) (Array Int) (Tensor a) (Tensor a)
slice :: forall a. Array Int -> Array Int -> Tensor a -> Tensor a
slice begin end = runFn3 sliceImpl begin end



foreign import transpose :: forall a. Tensor a -> Tensor a


foreign import toString :: forall a. Tensor a -> String
foreign import shape :: forall a. Tensor a -> Shape
foreign import rank :: forall a. Tensor a -> Int

foreign import zerosLike :: forall a. Tensor a -> Tensor Number



