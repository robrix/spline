module Main where

import Data.Distribution
import Data.List.NonEmpty
import Data.Semigroup ((<>))
import Linear.Affine
import Linear.V2 hiding (angle)
import Options.Applicative
import Spline.Drawing
import Spline.Walk

main :: IO ()
main = do
  (steps, n) <- execParser (info
    ((,) <$> option auto (long "steps" <> short 's' <> showDefault <> metavar "N" <> value 100)
         <*> option auto (long "walks" <> short 'w' <> showDefault <> metavar "N" <> value 50))
    (fullDesc <> progDesc "generative SVGs"))
  walks <- sample emptyEnv (wanderings steps n 0.01)
  putStrLn $ runDrawing Nothing $ do
    stroke Black
    fill Transparent
    snd $ foldr (\ walk (d, rest) -> (,) (d + dd) $ do
      path $ do
        move (P d)
        runWalk walk
      rest) (V2 100 100, return ()) walks
  where dd = V2 10 0


angle :: Distribution Float
angle = stdRandomR (negate pi) pi

wander :: Int -> Distribution (Walk Float ())
wander n = do
    phi <- angle
    momentum <- unitDistribution
    (face phi >>) . walk <$> foldr (\ _ next -> do
      phi <- angle * 0.001
      delta <- unitDistribution * 0.1
      Wander momentum angularMomentum rest <- next
      return (Wander (momentum + delta) (angularMomentum + phi) (turn angularMomentum >> step momentum >> rest))) (return (Wander momentum 0 (return ()))) [0..n]

data Wander a = Wander { momentum :: !a, angularMomentum :: !a, walk :: !(Walk a ()) }

wanderings :: Int -> Int -> Float -> Distribution [Walk Float ()]
wanderings steps n delta = toList <$> foldr (\ _ out -> do
  next :| rest <- out
  walk <- permute next (angle * pure delta)
  return (walk :| next : rest)) ((:| []) <$> wander steps) [0..n]
