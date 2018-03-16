module Data.Tensor.Typelevel where

import Data.Typelevel.Num.Sets
import Data.Typelevel.Num (D1, D0, class Pos, toInt, class Succ, class Mul, class Add)
import Data.Typelevel.Undefined
import Type.Proxy
import Prelude  -- ((<>), (<<<), class Semiring, one)
import Data.Newtype
import Data.Tensor as T
import Data.Functor.Compose
import Data.Foldable
import Unsafe.Coerce

-- A holder for dimension
newtype D n a = D a
derive instance newtypeEmailAddress :: Newtype (D n a) _

-- Raw or Safe rather than newtype
-- Compose does not let you newtype under it in one fell swoop
class GetItOut s t | s -> t where
  getOut :: s -> t
  getIn :: t -> s
-- Or just do unsafe operations. These are NOT for exporting.
-- They amount to unsafecoerces although only for types that are composed of D and Compose

instance getD :: GetItOut (D n a) a where
  getOut (D x) = x
  getIn x = D x
instance getCompose :: (GetItOut (f (g a)) (g a), GetItOut (g a) a) => GetItOut (Compose f g a) a where
  getOut (Compose x) = (getOut (getOut x))
  getIn x = Compose (getIn (getIn x))   

unD (D x) = x
--newtype DCompose f g a = DCompose (f (g a))
--derive instance newtypeDCompose :: Newtype (DCompose f g a) _

--type DProd n m a = DCompose (D n) (D m) a -- I probably need to newtype this

type DProd n m a = Compose (D n) (D m) a
infixr 6 type DProd as &*

--class Size (f :: Type -> Type) n


--instance sizeDCompose :: (Size f m, Mul n m p, Size g n) => Size (DCompose f g) p
--instance sizeD :: (Pos n) => Size (D n) n 

--instance (Mul n m p, Size f m) => Size (DCompose (D n) f) p where
-- Also rip off DProd

sized :: forall f. Shaped f => Proxy2 f -> Int
sized x = foldr mul one (shaped x)

-- rename to shaped
class Shaped f where
   shaped :: Proxy2 f -> T.Shape

instance shapedDCompose :: (Shaped f, Shaped g) => Shaped (Compose g f) where
   shaped _ = (shaped (Proxy2 :: Proxy2 g)) <> (shaped (Proxy2 :: Proxy2 f))

instance shapedD :: (Nat n) => Shaped (D n) where
   shaped _ =  [toInt (undefined :: n)]
-- I should keep shape as 

shape :: forall f a . Shaped f => f a -> T.Shape
shape _ = shaped (Proxy2 :: Proxy2 f)


type Any = D1 -- for broadcasting

-- shorthand for shape? Maybe some kind of punctuation is better? 
type Shp a = Proxy2 a
shp = Proxy2 

-- data Any -- Are there some situations where Any performs differently from 1?


--reshape :: NatEq n m, Size f n, Size g m => Proxy g -> f (Tensor a) -> g (Tensor a)
--reshape _ = unsafeCoerce <<< T.reshape <<< unsafeCoerce -- counting on newtypes having no runtime representation
-- OR. Better Yet, 
{-
reshape :: forall g f n a. Newtype (g (T.Tensor a)) (T.Tensor a) => Newtype (f (T.Tensor a)) (T.Tensor a) => Size f n => Size g n => Shaped g => Proxy2 g -> f (T.Tensor a) -> g (T.Tensor a)
reshape x = unsafeCoerce x
-}
ones :: forall f a. Semiring a => Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => Proxy2 f -> f (T.Tensor a)
ones _ = wrap (T.ones (shaped (Proxy2 :: Proxy2 f)))

zeros :: forall f a. Semiring a => Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => Proxy2 f -> f (T.Tensor a)
zeros _ = wrap (T.zeros (shaped (Proxy2 :: Proxy2 f)))

tconstructor :: forall f a. Shaped f => T.HasDType a => Newtype (f (T.Tensor a)) (T.Tensor a) => (T.Shape -> T.Tensor a) -> Proxy2 f -> f (T.Tensor a)
tconstructor f _ = wrap (f (shaped (Proxy2 :: Proxy2 f)))

-- zeros = tconstructor T.zeros

add' :: forall f a. Semiring a => Shaped f => Newtype (f (T.Tensor a)) (T.Tensor a) =>  f (T.Tensor a) -> f (T.Tensor a) -> f (T.Tensor a)
add' t1 t2 = wrap (T.addT (unwrap t1) (unwrap t2))

mul' :: forall f a. Semiring a => Shaped f => Newtype (f (T.Tensor a)) (T.Tensor a) =>  f (T.Tensor a) -> f (T.Tensor a) -> f (T.Tensor a)
mul' t1 t2 = wrap (T.mulT (unwrap t1) (unwrap t2))

-- this makes me uncomfortable
elementwiseBinOp :: forall a f. (T.Tensor a -> T.Tensor a -> T.Tensor a) -> (f (T.Tensor a) -> f (T.Tensor a) -> f (T.Tensor a))
elementwiseBinOp op t1 t2 =  unsafeCoerce (op (unsafeCoerce t1) (unsafeCoerce t2))
--add'' = elementwiseBinOp T.addT

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



class Shaped shape <= Rank shape n | shape -> n
instance composeRank :: (Rank f n, Rank g m, Add n m p) => Rank (Compose f g) p
instance rankD :: Nat n => Rank (D n) D1
class (Nat n, Shaped shape) <= Size shape n | shape -> n
instance sizeCompose :: (Size f n, Size g m, Mul n m p) => Rank (Compose f g) p
instance sizeD :: Nat n => Rank (D n) n

class (Nat axis, Shaped f, Shaped f') <= Reduce f axis f' | f axis -> f'
instance reduceRecurse :: (Shaped f, Shaped g, Reduce g x res, Succ x y) => Reduce (Compose f g) y (Compose f res)
instance reduceBase1 :: (Shaped f, Nat n) => Reduce (Compose (D n) f) D0 (Compose (D D1) f) 
instance reduceBase2 :: Nat n => Reduce (D n) D0 (D D1)


reduceSum :: forall axis n f g a. Shaped f => Shaped g => Reduce f axis g => axis -> f (T.Tensor a) -> g (T.Tensor a)
reduceSum _ x =  unsafeCoerce x -- (T.reduceSum [(toInt (undefined :: axis))] (unsafeCoerce x))

reshape :: forall g f n a. Size f n => Size g n => Proxy2 g -> f (T.Tensor a) -> g (T.Tensor a) -- Eq n n
reshape x = unsafeCoerce (T.reshape [1] (unsafeCoerce x))

{-
flatten :: Size f n => f (Tensor a) -> D n (Tensor a)
flatten = wrap <<< T.flatten <<< unwrap

fill :: (Shaped f) => HasDtype a -> a -> Proxy f -> f (Tensor a)
fill _ x = T.fill x (shape Proxy :: f)  -- (_dtype Proxy :: a)? Unless I already did this in Base tensor module

-- Do I have to export internally used typeclasses?
-- There is a useful map instance for D, but I don't want you to by able to apply to things underneath it outside this module
-- likwise for applicative
-- all the elmentwise and binary ops maintain shape and are basically 

-- Axis Selection is a total pain.
-- Enforce that axis is 



-- The tensor problem.




range start (Proxy) = 

--data Range n m a =  Range (Proxy2 (D n)) (Proxy2 (D m)) a

-- slicing
-}
