{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Control.Monad.Free.Freer
import Linear.V2 as Linear
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as S
import Text.Blaze.Svg.Renderer.Pretty as S

data DrawingF a where
  Move :: Real a => Linear.V2 a -> DrawingF ()

type Drawing = Freer DrawingF

-- Smart constructors

move :: Real a => V2 a -> Drawing ()
move p = Move p `Then` return


-- Running

runDrawing :: Real a => Linear.V2 a -> Drawing () -> String
runDrawing (V2 w h) = renderSvg . (docTypeSvg ! width (realValue w) ! height (realValue h)) . iterFreer algebra . fmap (const mempty)
  where algebra :: DrawingF x -> (x -> Svg) -> Svg
        algebra drawing cont = mempty

        realValue = stringValue . show . round . toRational
