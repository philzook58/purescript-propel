module Data.Tensor.Typelevel where

import Data.Typelevel.Num
{-
-- A holder for dimension
newtype D n a = D a

newtype DCompose f g a = DCompose (f (g a))
type DProd n m a = DCompose (D n) (D m) a -- I probably need to newtype this

infix 6 type DProd as &*

class Size f => Size (DCompose (D n) f) where
   size = mul
-- Also rip off DProd


size x = mul (shape x)

class Shaped f, Shaped g => Shaped (DCompose g f) where
   shape _ = shape g <> shape (Proxy1 :: f)

instance (Pos n) => Shaped (D n) where
   shape = pure (toInt Proxy :: n) 

instance Size f m, Mul n m p, Size g n => Size (DCompose f g) p


reshape :: NatEq n m, Size f n, Size g m => Proxy g -> f (Tensor a) -> g (Tensor a)
reshape _ = unsafeCoerce <<< T.reshape <<< unsafeCoerce -- counting on newtypes having no runtime representation
-- OR. Better Yet, 
reshape :: NewType g, NewType f, NatEq n m, Size f n, Size g m => Proxy g -> f (Tensor a) -> g (Tensor a)
reshape _ = wrap <<< T.reshape (shape Proxy :: g) <<< unwrap

flatten :: Size f n => f (Tensor a) -> D n (Tensor a)
flatten = wrap <<< T.flatten <<< unwrap

fill :: (Shaped f) => HasDtype a -> a -> Proxy f -> f (Tensor a)
fill _ x = T.fill x (shape Proxy :: f)  -- (_dtype Proxy :: a)? Unless I already did this in Base tensor module

-- Do I have to export internally used typeclasses?
-- There is a useful map instance for D, but I don't want you to by able to apply to things underneath it outside this module
-- likwise for applicative
-- all the elmentwise and binary ops maintain shape and are basically 



-- The tensor problem.

range start (Proxy) = 
 
-- slicing
-}
