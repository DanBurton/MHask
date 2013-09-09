{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Impl.Reader
  ( ReaderT
  , Monoid
  , fmap
  , extract
  ) where

import MHask.Arrow
import Prelude hiding (fmap)
import Data.Monoid
import Control.Monad.Trans.Reader

fmap :: (Monad m, Monad n)
  => (m ~> n) -> (ReaderT r m ~> ReaderT r n)
fmap f m = ReaderT (\r -> f (runReaderT m r))

extract :: (Monad m, Monoid r)
  => m <~ ReaderT r m
extract m = runReaderT m mempty
