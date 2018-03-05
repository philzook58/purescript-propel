module Data.Tensor.Util where

import Data.Tensor
import Data.Array (replicate, take, drop)
--import Data.List.Lazy hiding (replicate)
import Prelude

{-
Other useful operations:

powT
sinT
cosT
meshgrid

kron

Special matrics:
decimate -- multilpy on each side by subsampling matrix.
toeplitz
circulant


-}

unconcat :: forall a. Int -> Array a -> Array (Array a)
unconcat n [] = []
unconcat n xs = [(take n xs)] <> unconcat n (drop n xs) 


sadd :: Number -> Tensor Number -> Tensor Number
sadd = mapT addT


smul :: Number -> Tensor Number -> Tensor Number
--smult x t = mulT (pureLike x t) t
smul = mapT mulT

-- perhaps more asArray here?

-- As close as we can get to map probably using propel functions
-- somewhere between functor and applicative?

sOp :: forall a b c. (Tensor a -> Tensor b -> Tensor c) -> a -> Tensor b -> Tensor c
sOp f x  t = f (pureLike x t) t
mapT = sOp

-- should work with broadcasting.
pureLike :: forall a b. a -> Tensor b -> Tensor a
pureLike x t = fill x (replicate (rank t) 1)

{-
-- maybe lazy lists would be better. And then output the sequential approximation

horner :: List Number -> (Tensor Number -> Tensor Number)
horner (Cons c0 coeffs) t0 = foldr (\c t -> addT (pureLike c t0) (mulT t0 t)) (pureLike c0 t0) coeffs

cosT x = horner coscoeffs (square x)

coscoeffs = coscoeffs' (one:ints) one
ints = iterate (one + _) one
coscoeffs' (Cons x1 (Cons x2 xs)) acc = recip (x1 * x2 * acc) : coscoeffs' xs (neg acc)

-- Inifnite derivative chain
deriv f =  f : deriv (grad f)


-}