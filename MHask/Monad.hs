{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to base.Prelude.Monad
module MHask.Monad where

import Prelude hiding (Monad, join)
import qualified Prelude as P
import MHask.Util

import qualified MHask.Functor as MHask
import qualified MHask.Pointed as MHask

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


class (MHask.Pointed t) => Monad t where
  join :: (P.Monad m)
    => t (t m) ~> t m
  default join :: (P.Monad m, P.Monad (t m))
    => t (t m) ~> t m
  join = bind id

  bind :: (P.Monad m, P.Monad n)
    => (m ~> t n) -> (t m ~> t n)
  default bind :: (P.Monad m, P.Monad (t m),
                   P.Monad n, P.Monad (t n),
                   P.Monad (t (t n)))
    => (m ~> t n) -> (t m ~> t n)
  bind f = MHask.fmap f ~>~ join
