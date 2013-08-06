{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to comonad.Control.Comonad (Comonad)
module MHask.Comonad where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Functor as MHask
import qualified MHask.Copointed as MHask


class (MHask.Copointed t) => Comonad t where
  extend :: (Monad m, Monad n)
    => (t m ~> n) -> (t m ~> t n)
  default extend :: (Monad m, Monad n,
                     Monad (t m), Monad (t n),
                     Monad (t (t m)))
    => (t m ~> n) -> (t m ~> t n)
  extend f = duplicate ~>~ MHask.fmap f

  duplicate :: (Monad m)
    => t m ~> t (t m)
  default duplicate :: (Monad m, Monad (t m))
    => t m ~> t (t m)
  duplicate = extend id
