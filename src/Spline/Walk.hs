{-# LANGUAGE GADTs #-}
module Spline.Walk where

import Control.Monad.Free.Freer

data WalkF a f where
  Face :: a -> WalkF a ()
  Turn :: a -> WalkF a ()
  Forward :: a -> WalkF a ()

type Walk a = Freer (WalkF a)


-- Smart constructors

face :: a -> Walk a ()
face a = Face a `Then` return

turn :: a -> Walk a ()
turn a = Turn a `Then` return

forward :: a -> Walk a ()
forward a = Forward a `Then` return
