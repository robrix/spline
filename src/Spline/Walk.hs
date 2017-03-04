{-# LANGUAGE GADTs #-}
module Spline.Walk where

import Control.Monad.Free.Freer

data WalkF a f where
  Turn :: a -> WalkF a ()

type Walk a = Freer (WalkF a)


-- Smart constructors

turn :: a -> Walk a ()
turn a = Turn a `Then` return
