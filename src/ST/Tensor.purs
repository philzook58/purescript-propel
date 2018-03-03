module Data.ST.Tensor where

import Control.Monad.ST
import Data.Tensor
import Control.Monad.Eff (Eff)
import Prelude (Unit, class Ring, class Semiring, (<<<), pure)
import Unsafe.Coerce (unsafeCoerce)
import Data.Function.Uncurried

-- | based on Data.Array.ST

foreign import square :: forall h r a. (Semiring a) => STTensor h a -> Eff (st :: ST h | r) Unit -- (Tensor Number)
foreign import neg :: forall h r a. (Ring a) => STTensor h a -> Eff (st :: ST h | r) Unit
foreign import exp :: forall h r. STTensor h Number -> Eff (st :: ST h | r) Unit
foreign import log :: forall h r. STTensor h Number -> Eff (st :: ST h | r) Unit
foreign import sinh :: forall h r. STTensor h Number -> Eff (st :: ST h | r) Unit

foreign import flatten :: forall h r a. STTensor h a -> Eff (st :: ST h | r) Unit

--foreign import setDiag :: forall h r. STRef h (Tensor Number) -> (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

--foreign import reshapeImpl :: forall h r a. Fn2 Shape (STTensor h a) (Eff (st :: ST h | r) Unit)
--reshape :: forall h r a. Shape -> STTensor h a -> Eff (st :: ST h | r) Unit
--reshape shape t = runFn2 reshapeImpl shape t 

foreign import reverseImpl :: forall h r a. Fn2 (Array Int) (STTensor h a) (Eff (st :: ST h | r) Unit)
reverse :: forall h r a. Array Int -> STTensor h a -> Eff (st :: ST h | r) Unit
reverse dims t = runFn2 reverseImpl dims t 

foreign import data STTensor :: Type -> Type -> Type

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
unsafeFreeze :: forall a r h. STTensor h a -> Eff (st :: ST h | r) (Tensor a)
unsafeFreeze = pure <<< (unsafeCoerce :: STTensor h a -> Tensor a)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
unsafeThaw :: forall a r h. Tensor a -> Eff (st :: ST h | r) (STTensor h a)
unsafeThaw = pure <<< (unsafeCoerce :: Tensor a -> STTensor h a)


-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Tensor a -> Eff (st :: ST h | r) (STTensor h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STTensor h a -> Eff (st :: ST h | r) (Tensor a)
freeze = copyImpl

-- THIS CANNOT BE EXPORTED.
foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

{-
	Could make prebuilt STTensor verisons of these.
foreign import tensor :: Array Number -> Tensor Number
foreign import tensor2 :: Array (Array Number) -> Tensor Number
foreign import zeros :: Shape -> Tensor Number
foreign import ones :: Shape -> Tensor Number
foreign import eye :: Int -> Tensor Number
-}
{-

foreign import setDiag :: forall h r. STRef h (Tensor Number) -> (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)

foreign import reshape :: forall h r. Shape -> STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import flatten :: forall h r. STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number)
foreign import slice :: forall h r. Shape -> Shape -> STRef h (Tensor Number) -> Eff (st :: ST h | r) (Tensor Number) -- Should I call these types Shape?
-}