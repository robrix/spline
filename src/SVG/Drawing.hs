{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Control.Monad.Free.Freer
import Linear.V2 as Linear

data DrawingF a where
  Move :: Num a => Linear.V2 a -> DrawingF ()

type Drawing = Freer DrawingF
