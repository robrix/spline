{-# LANGUAGE GADTs #-}
module Spline.Path where

import Control.Monad.Free.Freer
import Linear.Affine
import Linear.V2 as Linear

data PathF a f where
  Move :: Point V2 a -> PathF a ()
  Line :: Point V2 a -> PathF a ()
  Cubic :: Point V2 a -> Point V2 a -> Point V2 a -> PathF a ()
  Close :: PathF a ()

type Path a = Freer (PathF a)


-- Smart constructors

move :: Point V2 a -> Path a ()
move p = Move p `Then` return

line :: Point V2 a -> Path a ()
line p = Line p `Then` return

close :: Path a ()
close = Close `Then` return

cubic :: Point V2 a -> Point V2 a -> Point V2 a -> Path a ()
cubic c1 c2 e = Cubic c1 c2 e `Then` return
