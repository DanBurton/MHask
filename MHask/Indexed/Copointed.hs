{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to indexed.Data.Functor.Indexed (IxCopointed)
module MHask.Indexed.Copointed where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Copointed as MHask
import qualified MHask.Indexed.Functor as MHask

class (MHask.IxFunctor t) => IxCopointed t where
  iextract :: (Monad m, Monad (t i i m))
    => t i i m ~> m
  default iextract :: (Monad m, Monad (t i i m), MHask.Copointed (t i i))
    => t i i m ~> m
  iextract = MHask.extract
