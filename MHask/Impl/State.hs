{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Impl.State
  ( StateT
  , Monoid
  , fmap
  , extract
  ) where

import Prelude hiding (fmap)
import Data.Monoid
import MHask.Arrow

import Control.Monad.Trans.State

fmap :: (Monad m, Monad n)
  => (m ~> n) -> (StateT s m ~> StateT s n)
fmap f tm = StateT $ \s -> f (runStateT tm s)

extract :: (Monad m, Monoid s)
  => m <~ StateT s m
extract tm = evalStateT tm mempty
