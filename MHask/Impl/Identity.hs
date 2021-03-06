{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Impl.Identity
  ( IdentityT
  , fmap
  , extract
  ) where

import Prelude hiding (fmap, return)
import MHask.Arrow

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class (lift)

fmap :: (Monad m, Monad n)
  => (m ~> n) -> (IdentityT m ~> IdentityT n)
fmap f m = IdentityT (f (runIdentityT m))

extract :: (Monad m)
  => m <~ IdentityT m
extract = runIdentityT

