{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}


-- Compare to todo.Copointed
module MHask.Copointed where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Monoid
import Control.Monad (liftM)

import qualified MHask.Functor as MHask


class (MHask.Functor t) => Copointed t where
  extract :: (Monad m)
    => t m ~> m


instance Copointed (WriterT w) where
  extract = liftM fst . runWriterT

instance (Monoid s) => Copointed (StateT s) where
  extract = flip evalStateT mempty

instance (Monoid r) => Copointed (ReaderT r) where
  extract = flip runReaderT mempty
