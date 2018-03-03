module Plot where

import DOM (DOM)
import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried
import Data.Tensor
{-
foreign import plotImpl :: forall e. Fn2 (Array (Tensor a)) (Array (Tensor a)) (Eff (dom::DOM | e) Unit)
plot :: forall e. Array (Tensor a) -> Array (Tensor a) -> Eff (dom::DOM | e) Unit
plot xs ys = runFn2 plotImpl xs ys 
-}
foreign import plot :: forall a e. Array (Tensor a) -> Eff ( dom::DOM | e) Unit

foreign import imshow :: forall a e. Tensor a -> Eff ( dom::DOM | e) Unit
