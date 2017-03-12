{-# LANGUAGE GADTs #-}
module Spline.Walk where

import Control.Monad.Free.Freer
import Control.Monad.State
import Data.Bifunctor
import Data.Functor.Classes
import Linear.Affine
import Linear.V2
import Spline.Path

data WalkF a f where
  Face :: a -> WalkF a ()
  Turn :: a -> WalkF a ()
  Step :: a -> WalkF a ()

type Walk a = Freer (WalkF a)


-- Smart constructors

face :: a -> Walk a ()
face a = Face a `Then` return

turn :: a -> Walk a ()
turn a = Turn a `Then` return

step :: a -> Walk a ()
step a = Step a `Then` return


-- Evaluation

runWalk :: Floating a => Walk a () -> Path a ()
runWalk = snd . flip execState (0, return ()) . iterFreerA algebra . void
  where algebra :: Floating a => WalkF a x -> (x -> State (a, Path a ()) ()) -> State (a, Path a ()) ()
        algebra walk cont = case walk of
          Face angle -> modify (first (const angle)) >> cont ()
          Turn angle -> modify (first (+ angle)) >> cont ()
          Step distance -> do
            (angle, path) <- get
            modify (second (>> lineR (polarToCartesian distance angle)))
            cont ()

polarToCartesian :: Floating a => a -> a -> V2 a
polarToCartesian r theta = V2 (r * cos theta) (r * sin theta)


-- Instances

instance Show a => Show1 (WalkF a) where
  liftShowsPrec _ _ d w = case w of
    Face a -> showsUnaryWith showsPrec "Face" d a
    Turn a -> showsUnaryWith showsPrec "Turn" d a
    Step a -> showsUnaryWith showsPrec "Step" d a
