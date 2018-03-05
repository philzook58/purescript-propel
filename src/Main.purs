module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tensor.Eff
import Data.Tensor
import DOM
import Data.Function.Uncurried
import Unsafe.Coerce
import Math
import Data.Maybe
import Data.Array
import Data.Tensor.Util
--import Data.TypedArray


foreign import plotlyImpl :: forall a e. Fn3 String (Array a) (Array a) (Eff (dom :: DOM | e) Unit)
plotly :: forall a e. String -> Array a -> Array a -> Eff (dom :: DOM | e) Unit
plotly = runFn3 plotlyImpl

plotT :: forall a e. String -> Tensor a -> Tensor a -> Eff (dom :: DOM | e) Unit
plotT divid x y = plotly divid (asArray x) (asArray y)


foreign import heatmapImpl :: forall a e. Fn2 String (Array (Array a)) (Eff (dom :: DOM | e) Unit)
heatmap :: forall a e. String -> Array (Array a) -> Eff (dom :: DOM | e) Unit
heatmap = runFn2 heatmapImpl
heatmapT divid z = heatmap divid (unconcat n (asArray z)) where n = fromMaybe 0 $ (shape z) !! 0

foreign import surfaceImpl :: forall a e. Fn2 String (Array (Array a)) (Eff (dom :: DOM | e) Unit)
surface :: forall a e. String -> Array (Array a) -> Eff (dom :: DOM | e) Unit
surface = runFn2 surfaceImpl
surfaceT divid z = surface divid (unconcat n (asArray z)) where n = fromMaybe 0 $ (shape z) !! 0



main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"
  log $ toString $ eye 7
  plotly "plot1" [1,2,3] [3,4,5]
  --let x = asArray $ dataSync (linspace 0.0 10.0 10)
  let x = (linspace 0.0 10.0 10) :: Tensor Number
  let x1 = (linspace (-3.14) 3.14 10)
  log $ unsafeCoerce x
  plotT "plot2" x x
  plotT "plot3" x (square x)
  plotT "plot4" x1 (tanh x1)
  plotly "plot4" (asArray x1) (map sin (asArray x1))
  let f x = square x
  let df = grad f
  plotT "plot5" x (df x) 
  let g x = mulT (mulT x x) x :: (Tensor Number)
  let dg = grad g
  plotT "plot5" x (dg x) 
  heatmapT "plot6" (eye 20)
  surfaceT "plot7" (eye 20)

