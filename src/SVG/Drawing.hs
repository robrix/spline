{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Linear.V2 as Linear

data DrawingF a where
  Move :: Num a => Linear.V2 a -> DrawingF ()
