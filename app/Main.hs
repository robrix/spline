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
  putStrLn $ runDrawing (V2 200 200) $ do
    stroke Black
    fill Transparent
    path $ do
      moveR (V2 100 100)
      runWalk walk


angle :: Distribution Float
angle = stdRandomR (negate pi) pi

wander :: Int -> Distribution (Walk Float ())
wander n = do
    phi <- angle
    (face phi >>) <$> turnStep n
  where turnStep n
          | n <= 0 = return (return ())
          | otherwise = do
            t <- turn <$> (angle * 0.1)
            s <- (t >>) . step <$> 15
            (s >>) <$> wander (pred n)
