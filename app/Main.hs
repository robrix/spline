module Main where

import Data.Distribution
import Data.Kind
import Linear.Affine
import Linear.V2 hiding (angle)
import Spline.Drawing
import Spline.Walk

main :: IO ()
main = do
  walk <- sample emptyEnv (wander 5)
  walk2 <- sample emptyEnv (permute walk (angle * 0.01))
  putStrLn $ runDrawing (V2 200 200) $ do
    stroke Black
    fill Transparent
    path $ do
      moveR (V2 100 100)
      runWalk walk
    path $ do
      moveR (V2 100 100)
      runWalk walk2


angle :: Distribution Float
angle = stdRandomR (negate pi) pi

wander :: Int -> Distribution (Walk Float ())
wander n = do
    phi <- angle
    (face phi >>) <$> foldr (\ _ into -> do
      phi <- angle * 0.1
      (turn phi >> step 15 >>) <$> into) (return (return ())) [0..n]
