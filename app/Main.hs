module Main where

import Data.Distribution
import Data.List.NonEmpty
import Linear.Affine
import Linear.V2 hiding (angle)
import Spline.Drawing
import Spline.Walk

main :: IO ()
main = do
  walks <- sample emptyEnv (wanderings 20 50 0.01)
  putStrLn $ runDrawing Nothing $ do
    stroke Black
    fill Transparent
    path . snd $ foldr (\ walk (V2 dx dy, rest) -> (,) (V2 (dx + ddx) (dy + ddy)) $ do
      move (P (V2 (100 + dx) (100 + dy)))
      runWalk walk
      rest) (V2 0 0, return ()) walks
  where V2 ddx ddy = V2 10 0


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
