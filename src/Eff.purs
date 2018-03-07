module Data.Tensor.Eff where

import DOM (DOM)
import Prelude
import Control.Monad.Eff (Eff)
import Data.Tensor (Tensor)
{-
	-- These functions appear to not work.
foreign import plot :: forall a e. Array (Tensor a) -> Eff ( dom::DOM | e) Unit

foreign import imshow :: forall a e. Tensor a -> Eff ( dom::DOM | e) Unit
-}
foreign import dispose :: forall a e. Tensor a -> Eff e Unit
