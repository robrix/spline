module Main where

import Linear.V2
import Spline.Drawing

main :: IO ()
main = putStrLn $ runDrawing (V2 200 200) $ do
  stroke Black
  fill Transparent
  path $ do
    move (V2 10 10)
    line (V2 20 20)
    cubic (V2 20 40) (V2 30 30) (V2 50 10)
