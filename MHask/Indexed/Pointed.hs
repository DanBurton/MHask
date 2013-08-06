{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to indexed.Data.Functor.Indexed (IxPointed)
module MHask.Indexed.Pointed where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Pointed as MHask
import qualified MHask.Indexed.Functor as MHask

class (MHask.IxFunctor t) => IxPointed t where
  ireturn :: (Monad m)
    => m ~> t i i m
  default ireturn :: (Monad m,
                      MHask.Pointed (t i i))
    => m ~> t i i m
  ireturn = MHask.return
