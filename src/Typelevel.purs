module Data.Tensor.Typelevel where

import Data.Typelevel.Num
import Data.Typelevel.Undefined
import Type.Proxy
import Prelude ((<>), (<<<), class Semiring)
import Data.Newtype
import Data.Tensor as T

-- A holder for dimension
newtype D n a = D a
derive instance newtypeEmailAddress :: Newtype (D n a) _

newtype DCompose f g a = DCompose (f (g a))
derive instance newtypeDCompose :: Newtype (DCompose f g a) _

type DProd n m a = DCompose (D n) (D m) a -- I probably need to newtype this

infix 6 type DProd as &*

class Size (f :: Type -> Type) n

instance sizeDCompose :: (Size f m, Mul n m p, Size g n) => Size (DCompose f g) p
instance sizeD :: (Pos n) => Size (D n) n 
--instance (Mul n m p, Size f m) => Size (DCompose (D n) f) p where
-- Also rip off DProd


size x = mul (shape x)

class Shaped f where
   shape :: Proxy2 f -> Array Int

instance shapedDCompose :: (Shaped f, Shaped g) => Shaped (DCompose g f) where
   shape _ = (shape (Proxy2 :: Proxy2 g)) <> (shape (Proxy2 :: Proxy2 f))

instance shapedD :: (Pos n) => Shaped (D n) where
   shape _ =  [toInt (undefined :: n)]




--reshape :: NatEq n m, Size f n, Size g m => Proxy g -> f (Tensor a) -> g (Tensor a)
--reshape _ = unsafeCoerce <<< T.reshape <<< unsafeCoerce -- counting on newtypes having no runtime representation
-- OR. Better Yet, 

reshape :: forall g f n a. Newtype (g (T.Tensor a)) (T.Tensor a) => Newtype (f (T.Tensor a)) (T.Tensor a) => Size f n => Size g n => Shaped g => Proxy2 g -> f (T.Tensor a) -> g (T.Tensor a)
reshape _ = wrap <<< T.reshape (shape (Proxy2 :: Proxy2 g)) <<< unwrap

ones :: forall f a. Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => Proxy2 f -> f (T.Tensor a)
ones _ = wrap (T.ones (shape (Proxy2 :: Proxy2 f)))

zeros :: forall f a. Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => Proxy2 f -> f (T.Tensor a)
zeros _ = wrap (T.zeros (shape (Proxy2 :: Proxy2 f)))

tconstructor :: forall f a. Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => (T.Shape -> T.Tensor a) -> Proxy2 f -> f (T.Tensor a)
tconstructor f _ = wrap (f (shape (Proxy2 :: Proxy2 f)))

-- zeros = tconstructor T.zeros

add' :: forall f a. Semiring a => Shaped f => Newtype (f (T.Tensor a)) (T.Tensor a) =>  f (T.Tensor a) -> f (T.Tensor a) -> f (T.Tensor a)
add' t1 t2 = wrap (T.addT (unwrap t1) (unwrap t2))

mul' :: forall f a. Semiring a => Shaped f => Newtype (f (T.Tensor a)) (T.Tensor a) =>  f (T.Tensor a) -> f (T.Tensor a) -> f (T.Tensor a)
mul' t1 t2 = wrap (T.mulT (unwrap t1) (unwrap t2))

newtype ITensor a = ITensor a
-- ITensor n a 
{- -- orphan instance problems
	-- lordy.
	-- So I guess I'll need to... Put a total newtype everound everything?
	-- The explicit phantom indexed version is looking less goofy now


instance semiringITensor :: (Shaped f, Semiring a) => Semiring (f (T.Tensor a)) where
   zero = zeros (Proxy2 :: Proxy2 f)
   one = ones (Proxy2 :: Proxy2 f)
   add = add'
   mul = mul'
-}

{-
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
