{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to indexed.Control.Monad.Indexed (IxMonad)
module MHask.Indexed.Monad where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Monad as MHask
import qualified MHask.Indexed.Pointed as MHask
import qualified MHask.Indexed.Functor as MHask

class (MHask.IxPointed t) => IxMonad t where
  ijoin :: (Monad m, Monad (t i j (t j k m)), Monad (t i k m))
    => t i j (t j k m) ~> t i k m
  default ijoin :: (Monad m, Monad (t i j (t j k m)), Monad (t i k m),
                     Monad (t j k m))
    => t i j (t j k m) ~> t i k m
  ijoin = ibind id


  ibind :: (Monad m, Monad (t i j m), Monad n, Monad (t j k n))
    => (m ~> t j k n) -> (t i j m ~> t i k n)
  default ibind :: (Monad m, Monad (t i j m), Monad n, Monad (t j k n),
             Monad (t i k n), Monad (t i j (t j k n)))
    => (m ~> t j k n) -> (t i j m ~> t i k n)
  ibind f = MHask.imap f ~> ijoin
