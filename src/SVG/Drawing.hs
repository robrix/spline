{-# LANGUAGE GADTs #-}
module SVG.Drawing
( Drawing()
, path
, runDrawing
, module SVG.Path
) where

import Control.Monad.Free.Freer
import Linear.V2 as Linear
import SVG.Path
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 as S ((!))
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Text.Blaze.Svg.Renderer.Pretty as S

data DrawingF a f where
  Path :: Path a () -> DrawingF a ()

type Drawing a = Freer (DrawingF a)


-- Smart constructors

path :: Path a () -> Drawing a ()
path p = Path p `Then` return


-- Running

runDrawing :: Real a => Linear.V2 a -> Drawing a () -> String
runDrawing (V2 w h) = S.renderSvg . (S.docTypeSvg ! A.width (realValue w) ! A.height (realValue h)) . iterFreer algebra . fmap (const mempty)
  where algebra :: DrawingF a x -> (x -> S.Svg) -> S.Svg
        algebra drawing cont = case drawing of
          Path p -> S.path ! A.d (iterFreer renderPath (mempty <$ p))

        renderPath :: PathF a x -> (x -> S.AttributeValue) -> S.AttributeValue
        renderPath path cont = mempty

        realValue = S.stringValue . show . round . toRational
