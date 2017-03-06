{-# LANGUAGE GADTs #-}
module Spline.Path where

import Control.Monad.Free.Freer
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
