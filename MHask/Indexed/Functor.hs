{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to indexed.Data.Functor.Indexed (IxFunctor)
module MHask.Indexed.Functor where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Functor as MHask

class IxFunctor t where
  imap :: (Monad m, Monad n, Monad (t i j m), Monad (t i j n))
    => (m ~> n) -> (t i j m ~> t i j n)
  default imap :: (Monad m, Monad n, Monad (t i j m), Monad (t i j n),
                     MHask.Functor (t i j))
    => (m ~> n) -> (t i j m ~> t i j n)
  imap = MHask.fmap
