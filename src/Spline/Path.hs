{-# LANGUAGE GADTs #-}
module Spline.Path where

import Control.Monad.Free.Freer
import Linear.V2 as Linear

data PathF a f where
  Move :: V2 a -> PathF a ()
  Line :: V2 a -> PathF a ()
  Cubic :: V2 a -> V2 a -> V2 a -> PathF a ()
  Close :: PathF a ()

type Path a = Freer (PathF a)


-- Smart constructors

move :: V2 a -> Path a ()
move p = Move p `Then` return

line :: V2 a -> Path a ()
line p = Line p `Then` return

close :: Path a ()
close = Close `Then` return

cubic :: V2 a -> V2 a -> V2 a -> Path a ()
cubic c1 c2 e = Cubic c1 c2 e `Then` return
