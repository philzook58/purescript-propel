module Data.Tensor.Aff where

import Control.Monad.Aff
import Control.Monad.Aff.Compat
import Data.ArrayBuffer.Types (Float32Array)
import Data.Tensor
import Prelude

foreign import _data :: forall eff. Tensor Number -> EffFnAff eff Float32Array

getData :: forall eff. Tensor Number -> Aff eff Float32Array
getData = fromEffFnAff <<< _data