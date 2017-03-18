module Main where

import Data.Distribution
import Data.List.NonEmpty
import Linear.Affine
import Linear.V2 hiding (angle)
import Spline.Drawing
import Spline.Walk

main :: IO ()
main = do
  walks <- sample emptyEnv (wanderings 5 10 0.01)
  putStrLn $ runDrawing (V2 200 200) $ do
    stroke Black
    fill Transparent
    path $ foldr (\ walk rest -> do
      move (P (V2 100 100))
      runWalk walk
      rest) (return ()) walks


angle :: Distribution Float
angle = stdRandomR (negate pi) pi

wander :: Int -> Distribution (Walk Float ())
wander n = do
    phi <- angle
    (face phi >>) <$> foldr (\ _ into -> do
      phi <- angle * 0.1
      (turn phi >> step 15 >>) <$> into) (return (return ())) [0..n]

wanderings :: Int -> Int -> Float -> Distribution [Walk Float ()]
wanderings steps n delta = toList <$> foldr (\ _ out -> do
  next :| rest <- out
  walk <- permute next (angle * pure delta)
  return (walk :| next : rest)) ((:| []) <$> wander steps) [0..n]
