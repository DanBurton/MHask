{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}


-- | Equivalent to transformers.Control.Monad.Trans.Class (MonadTrans)
module MHask.Pointed where

import Prelude hiding (return)
import MHask.Util

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Functor as MHask



-- | The dual of "MHask.Copointed"
class (MHask.Functor t) => Pointed t where
  return :: (Monad m)
    => m ~> t m


instance Pointed (StateT s) where
  return mx = StateT $ \s -> liftM (\x -> (x, s)) mx

instance Pointed (ReaderT r) where
  return mx = ReaderT $ \_ -> mx

instance Monoid w => Pointed (WriterT w) where
  return mx = WriterT $ liftM (\x -> (x, mempty)) mx
