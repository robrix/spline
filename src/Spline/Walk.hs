{-# LANGUAGE GADTs, ScopedTypeVariables #-}
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


-- Transformation

permute :: forall a f. (Applicative f, Num a) => Walk a () -> f a -> f (Walk a ())
permute walk byA = iterFreerA algebra (return () <$ walk)
  where algebra :: WalkF a x -> (x -> f (Walk a ())) -> f (Walk a ())
        algebra walk cont = case walk of
          Face a -> adding face a <$> byA <*> cont ()
          Turn a -> adding turn a <$> byA <*> cont ()
          Step a -> adding step a <$> byA <*> cont ()
        adding with a by = (with (a + by) >>)


-- Evaluation

runWalk :: Floating a => Walk a () -> Path a ()
runWalk = snd . flip execState (0, return ()) . iterFreerA algebra . void
  where algebra :: Floating a => WalkF a x -> (x -> State (a, Path a ()) ()) -> State (a, Path a ()) ()
        algebra walk cont = case walk of
          Face angle -> modify (first (const angle)) >> cont ()
          Turn angle -> modify (first (+ angle)) >> cont ()
          Step distance -> modify (\ (angle, path) -> (angle, path >> lineR (polarToCartesian distance angle))) >> cont ()

polarToCartesian :: Floating a => a -> a -> V2 a
polarToCartesian r theta = V2 (r * cos theta) (r * sin theta)


-- Instances

instance Show a => Show1 (WalkF a) where
  liftShowsPrec _ _ d w = case w of
    Face a -> showsUnaryWith showsPrec "Face" d a
    Turn a -> showsUnaryWith showsPrec "Turn" d a
    Step a -> showsUnaryWith showsPrec "Step" d a
