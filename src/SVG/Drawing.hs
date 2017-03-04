{-# LANGUAGE GADTs #-}
module SVG.Drawing
( Drawing()
, path
, runDrawing
, module SVG.Path
) where

import Control.Monad.Free.Freer
import Control.Monad.State
import Data.Function
import Linear.V2 as Linear
import SVG.Path
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 as S ((!))
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Text.Blaze.Svg.Renderer.Pretty as S

data Colour a = Black | White | Transparent

data DrawingF a f where
  Fill :: Colour a -> DrawingF a ()
  Path :: Path a () -> DrawingF a ()

type Drawing a = Freer (DrawingF a)


-- Smart constructors

path :: Path a () -> Drawing a ()
path p = Path p `Then` return

fill :: Colour a -> Drawing a ()
fill c = Fill c `Then` return


-- Running

runDrawing :: (Real a, Show a) => Linear.V2 a -> Drawing a () -> String
runDrawing (V2 w h) = S.renderSvg . (S.docTypeSvg ! A.width (realValue w) ! A.height (realValue h)) . flip evalState Nothing . iterFreer algebra . fmap (const (return mempty))
  where algebra :: Show a => DrawingF a x -> (x -> State (Maybe (Colour a)) S.Svg) -> State (Maybe (Colour a)) S.Svg
        algebra drawing cont = case drawing of
          Fill c -> put (Just c) >> return mempty
          Path p -> do
            fill <- get
            return $ S.path ! A.d (S.mkPath (iterFreer renderPath (return () <$ p))) & case fill of { Just fill -> (! A.fill (renderColour fill)) ; _ -> id }

        renderPath :: Show a => PathF a x -> (x -> S.Path) -> S.Path
        renderPath path cont = case path of
          Move (V2 x y) -> S.m x y >> cont ()
          Line (V2 x y) -> S.l x y >> cont ()

        renderColour :: Colour a -> S.AttributeValue
        renderColour colour = case colour of
          Black -> S.stringValue "black"
          White -> S.stringValue "white"
          Transparent -> S.stringValue "transparent"

        realValue = S.stringValue . show . round . toRational
