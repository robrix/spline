module Main where

import Linear.V2
import SVG.Drawing

main :: IO ()
main = putStrLn $ runDrawing (V2 200 200) $ do
  return ()
