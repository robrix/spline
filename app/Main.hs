module Main where

import Data.Distribution
import Data.Kind
import Linear.Affine
import Linear.V2 hiding (angle)
import Spline.Drawing
import Spline.Walk

main :: IO ()
main = do
  p <- sample emptyEnv wander
  putStrLn $ runDrawing (V2 200 200) $ do
          stroke Black
          fill Transparent
          path $ do
            moveR (V2 100 100)
            p


angle :: Distribution Float
angle = stdRandomR (negate pi) pi

polarToCartesian :: Floating a => a -> a -> V2 a
polarToCartesian r theta = V2 (r * cos theta) (r * sin theta)

wander :: Distribution (Path Float ())
wander = do
  theta <- angle
  next <- angle * 0.1
  pure $! do
    lineR (polarToCartesian 15 theta)
    lineR (polarToCartesian 15 (next + theta))
