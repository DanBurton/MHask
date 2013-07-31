{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

-- Compare to base.Prelude.Functor (Functor)
module MHask.Functor where

import Prelude hiding (Functor, fmap)
import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


class Functor t where
  fmap :: (Monad m, Monad n, Monad (t m), Monad (t n))
    => (m ~> n) -> (t m ~> t n)


instance Functor (StateT s) where
  fmap f m = StateT $ \s -> f (runStateT m s)

instance Functor (ReaderT r) where
  fmap f m = ReaderT $ \r -> f (runReaderT m r)

instance Functor (WriterT w) where
  fmap f m = WriterT $ f (runWriterT m)

