{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

-- no relevant comparison
module MHask.Pointed where

import Prelude hiding (return)
import MHask.Util

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import qualified MHask.Functor as MHask

class (MHask.Functor t) => Pointed t where
  return :: (Monad m)
    => (m ~> t m)


-- This is basically MonadTrans
-- lift = MHask.return

instance Pointed (StateT s) where
  return mx = StateT $ \s -> liftM (\x -> (x, s)) mx

instance Pointed (ReaderT r) where
  return mx = ReaderT $ \_ -> mx

instance Monoid w => Pointed (WriterT w) where
  return mx = WriterT $ liftM (\x -> (x, mempty)) mx
