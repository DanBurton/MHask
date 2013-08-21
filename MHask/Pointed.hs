{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Equivalent to transformers.Control.Monad.Trans.Class (MonadTrans)
module MHask.Pointed where

import Prelude hiding (return)
import MHask.Arrow

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


import qualified MHask.Duplicate as MHask


-- | The dual of "MHask.Copointed"
class (MHask.Duplicate t) => Pointed t where
  -- | Instances must obey the following laws:
  -- 
  -- > duplicate === return :: t m ~> t (t m)
  -- > return ~>~ fmap f ≡ f ~>~ return
  return :: (Monad m)
    => m ~> t m





{-
instance Pointed (StateT s) where
  return mx = StateT $ \s -> liftM (\x -> (x, s)) mx

instance Pointed (ReaderT r) where
  return mx = ReaderT $ \_ -> mx

instance Monoid w => Pointed (WriterT w) where
  return mx = WriterT $ liftM (\x -> (x, mempty)) mx
-}

