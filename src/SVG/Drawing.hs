{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Control.Monad.Free.Freer
import Linear.V2 as Linear

data DrawingF a where
  Move :: Real a => Linear.V2 a -> DrawingF ()

type Drawing = Freer DrawingF

runDrawing :: Real a => Linear.V2 a -> Drawing () -> ShowS
runDrawing size = iterFreer algebra . fmap (const id)
  where algebra :: DrawingF x -> (x -> ShowS) -> ShowS
        algebra drawing cont = id
