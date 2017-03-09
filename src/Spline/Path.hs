{-# LANGUAGE GADTs #-}
module Spline.Path where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Linear.Affine
import Linear.V2 as Linear

data PathF a f where
  Move :: Point V2 a -> PathF a ()
  MoveR :: V2 a -> PathF a ()
  Line :: Point V2 a -> PathF a ()
  LineR :: V2 a -> PathF a ()
  Cubic :: Point V2 a -> Point V2 a -> Point V2 a -> PathF a ()
  CubicR :: V2 a -> V2 a -> V2 a -> PathF a ()
  Close :: PathF a ()

type Path a = Freer (PathF a)


-- Smart constructors

move :: Point V2 a -> Path a ()
move p = Move p `Then` return

moveR :: V2 a -> Path a ()
moveR v = MoveR v `Then` return

line :: Point V2 a -> Path a ()
line p = Line p `Then` return

lineR :: V2 a -> Path a ()
lineR v = LineR v `Then` return

cubic :: Point V2 a -> Point V2 a -> Point V2 a -> Path a ()
cubic c1 c2 e = Cubic c1 c2 e `Then` return

cubicR :: V2 a -> V2 a -> V2 a -> Path a ()
cubicR c1 c2 e = CubicR c1 c2 e `Then` return

close :: Path a ()
close = Close `Then` return


-- Instances

instance Show a => Show1 (PathF a) where
  liftShowsPrec _ _ d w = case w of
    Move a -> showsUnaryWith showsPrec "Move" d a
    MoveR a -> showsUnaryWith showsPrec "MoveR" d a
    Line a -> showsUnaryWith showsPrec "Line" d a
    LineR a -> showsUnaryWith showsPrec "LineR" d a
    Cubic c1 c2 p -> showsTernaryWith showsPrec showsPrec showsPrec "Cubic" d c1 c2 p
    CubicR c1 c2 p -> showsTernaryWith showsPrec showsPrec showsPrec "CubicR" d c1 c2 p
    Close -> showString "Close"
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
            showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
