{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Equivalent to transformers.Control.Monad.Trans.Class (MonadTrans)
module MHask.Pointed where

import Prelude hiding (return)
import MHask.Arrow
import Control.Monad.Trans.Class

import qualified MHask.Duplicate as MHask

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R

-- | Dual of "MHask.Copointed"
class (MHask.Duplicate t) => Pointed t where
  -- | Instances must obey the following laws:
  -- 
  -- > duplicate ≡ return :: t m ~> t (t m)
  -- > return ~>~ fmap f ≡ f ~>~ return
  return :: (Monad m)
    => m ~> t m
  default return :: (Monad m, MonadTrans t)
    => m ~> t m
  return = lift


instance Pointed I.IdentityT
instance Pointed (S.StateT s)
instance Pointed (R.ReaderT r)
