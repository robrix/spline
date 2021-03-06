{-# LANGUAGE GADTs #-}
module Spline.Drawing
( Colour(..)
, Drawing()
, fill
, stroke
, path
, runDrawing
, module Spline.Path
) where

import Control.Monad.Free.Freer
import Control.Monad.State
import Data.Function
import Data.Functor.Classes
import Linear.Affine as Linear
import Linear.V2 as Linear
import Spline.Path
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 as S ((!))
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Text.Blaze.Svg.Renderer.Pretty as S

data Colour a = Black | White | Transparent
  deriving Show

data DrawingF a f where
  Fill :: Colour a -> DrawingF a ()
  Stroke :: Colour a -> DrawingF a ()
  Path :: Path a () -> DrawingF a ()

type Drawing a = Freer (DrawingF a)


-- Smart constructors

fill :: Colour a -> Drawing a ()
fill c = Fill c `Then` return

stroke :: Colour a -> Drawing a ()
stroke c = Stroke c `Then` return

path :: Path a () -> Drawing a ()
path p = Path p `Then` return


-- Running

runDrawing :: (Real a, Show a) => Maybe (V2 a) -> Drawing a () -> String
runDrawing size = S.renderSvg . (S.docTypeSvg !? (A.width . realValue . w <$> size) !? (A.height . realValue . h <$> size)) . flip evalState (DrawingState Nothing Nothing) . iterFreer algebra . fmap (const (return mempty))
  where algebra :: Show a => DrawingF a x -> (x -> State (DrawingState a) S.Svg) -> State (DrawingState a) S.Svg
        algebra drawing cont = case drawing of
          Fill c -> modify (setFillColour (Just c)) >> cont ()
          Stroke c -> modify (setStrokeColour (Just c)) >> cont ()
          Path p -> do
            state <- get
            mappend (S.path ! A.d (S.mkPath (iterFreer renderPath (return () <$ p))) !? (A.fill . renderColour <$> fillColour state) !? (A.stroke . renderColour <$> strokeColour state)) <$> cont ()

        (!?) e = maybe e (e !)

        renderPath :: Show a => PathF a x -> (x -> S.Path) -> S.Path
        renderPath path cont = case path of
          Move (P (V2 x y)) -> S.m x y >> cont ()
          MoveR (V2 x y) -> S.mr x y >> cont ()
          Line (P (V2 x y)) -> S.l x y >> cont ()
          LineR (V2 x y) -> S.lr x y >> cont ()
          Cubic (P (V2 c1x c1y)) (P (V2 c2x c2y)) (P (V2 x y)) -> S.c c1x c1y c2x c2y x y
          CubicR (V2 c1x c1y) (V2 c2x c2y) (V2 x y) -> S.cr c1x c1y c2x c2y x y

        renderColour :: Colour a -> S.AttributeValue
        renderColour colour = case colour of
          Black -> S.stringValue "black"
          White -> S.stringValue "white"
          Transparent -> S.stringValue "transparent"

        realValue = S.stringValue . show . round . toRational
        w (V2 w _) = w
        h (V2 _ h) = h


data DrawingState a = DrawingState { fillColour :: Maybe (Colour a), strokeColour :: Maybe (Colour a) }

setFillColour :: Maybe (Colour a) -> DrawingState a -> DrawingState a
setFillColour c s = s { fillColour = c }

setStrokeColour :: Maybe (Colour a) -> DrawingState a -> DrawingState a
setStrokeColour c s = s { strokeColour = c }


-- Instance

instance Show a => Show1 (DrawingF a) where
  liftShowsPrec _ _ d w = case w of
    Fill a -> showsUnaryWith showsPrec "Fill" d a
    Stroke a -> showsUnaryWith showsPrec "Stroke" d a
    Path a -> showsUnaryWith showsPrec "Path" d a
