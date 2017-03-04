module Main where

import Linear.V2
import SVG.Drawing

main :: IO ()
main = putStrLn $ runDrawing (V2 200 200) $ do
  fill Black
  path $ do
    move (V2 10 10)
    line (V2 20 20)
