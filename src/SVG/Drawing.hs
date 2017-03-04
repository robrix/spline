{-# LANGUAGE GADTs #-}
module SVG.Drawing where

import Control.Monad.Free.Freer
import Linear.V2 as Linear
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as S
import Text.Blaze.Svg.Renderer.Pretty as S

data DrawingF a f where
  Move :: Linear.V2 a -> DrawingF a ()
  Line :: Linear.V2 a -> DrawingF a ()

type Drawing a = Freer (DrawingF a)

-- Smart constructors

move :: V2 a -> Drawing a ()
move p = Move p `Then` return

line :: V2 a -> Drawing a ()
line p = Line p `Then` return


-- Running

runDrawing :: Real a => Linear.V2 a -> Drawing a () -> String
runDrawing (V2 w h) = renderSvg . (docTypeSvg ! width (realValue w) ! height (realValue h)) . iterFreer algebra . fmap (const mempty)
  where algebra :: DrawingF a x -> (x -> Svg) -> Svg
        algebra drawing cont = mempty

        realValue = stringValue . show . round . toRational
