{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Impl.Writer
  ( WriterT
  , runWriterT
  , Monoid
  , fmap
  , extract
  ) where

import Prelude hiding (fmap)
import Data.Monoid
import MHask.Arrow

import Control.Monad.Trans.Writer

fmap :: (Monad m, Monad n)
  => (m ~> n) -> (WriterT w m ~> WriterT w n)
fmap f tm = WriterT $ f (runWriterT tm)

extract :: (Monad m)
  => m <~ WriterT w m
extract m = do -- Why is there no evalWriterT ?
  ~(a, _) <- runWriterT m
  return a
