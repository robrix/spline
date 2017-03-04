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

runDrawing :: (Real a, Show a) => Linear.V2 a -> Drawing a () -> String
runDrawing (V2 w h) = S.renderSvg . (S.docTypeSvg ! A.width (realValue w) ! A.height (realValue h)) . iterFreer algebra . fmap (const mempty)
  where algebra :: Show a => DrawingF a x -> (x -> S.Svg) -> S.Svg
        algebra drawing cont = case drawing of
          Path p -> S.path ! A.d (S.mkPath (iterFreer renderPath (return () <$ p)))

        renderPath :: Show a => PathF a x -> (x -> S.Path) -> S.Path
        renderPath path cont = case path of
          Move (V2 x y) -> S.m x y >> cont ()

        realValue = S.stringValue . show . round . toRational
