{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Control.Monad.Free.Freer
import Linear.V2 as Linear
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg.Renderer.Pretty as S

data DrawingF a where
  Move :: Real a => Linear.V2 a -> DrawingF ()

type Drawing = Freer DrawingF

runDrawing :: Real a => Linear.V2 a -> Drawing () -> String
runDrawing size = renderSvg . docTypeSvg . iterFreer algebra . fmap (const mempty)
  where algebra :: DrawingF x -> (x -> Svg) -> Svg
        algebra drawing cont = mempty
