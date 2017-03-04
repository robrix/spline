{-# LANGUAGE GADTs #-}
module Spline.Walk where

import Control.Monad.Free.Freer

data WalkF a f where
  Turn :: a -> WalkF a ()

type Walk a = Freer (WalkF a)