{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- Compare to indexed.Control.Comonad.Indexed (IxComonad)
module MHask.Indexed.Comonad where

import MHask.Util

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Indexed.Functor as MHask
import qualified MHask.Indexed.Copointed as MHask


class (MHask.IxCopointed t) => IxComonad t where
  iextend :: (Monad m, Monad n,
             Monad (t i j m), Monad (t j k n), Monad (t i k m))
    => (t j k m ~> n) -> (t i k m ~> t i j n)
  default iextend :: (Monad m, Monad n,
             Monad (t i j m), Monad (t j k n), Monad (t i k m),
             Monad (t i j (t j k m)), Monad (t j k m), Monad (t i j n))
    => (t j k m ~> n) -> (t i k m ~> t i j n)
  iextend f = iduplicate ~> MHask.imap f

  iduplicate :: (Monad m, Monad (t i k m), Monad (t i j (t j k m)))
    => t i k m ~> t i j (t j k m)

  default iduplicate :: (Monad m, Monad (t i k m), Monad (t i j (t j k m)),
                          (Monad (t j k (t j k m))), (Monad (t i j m)),  (Monad (t j k m)))
    => t i k m ~> t i j (t j k m)
  iduplicate = iextend id
