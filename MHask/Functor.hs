{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to base.Prelude.Functor (Functor)
module MHask.Functor where

import Prelude hiding (Functor, fmap)
import MHask.Arrow

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R
import qualified MHask.Impl.Writer   as W




-- | Functor is its own dual.
class Functor t where
  -- | Flipping the arrows on fmap's type signature
  -- is just the same type signature in disguise.
  -- 
  -- > (m <~ n) -> (t m <~ t n)
  -- 
  -- Any implementation of @fmap@ must obey Functor laws.
  -- 
  -- > fmap identityArrow ≡ identityArrow
  -- > fmap (f ~>~ g)     ≡ fmap f ~>~ fmap g
  fmap :: (Monad m, Monad n)
    => (m ~> n) -> (t m ~> t n)






instance Functor I.IdentityT where
  fmap = I.fmap

instance Functor (S.StateT s) where
  fmap = S.fmap

instance Functor (R.ReaderT r) where
  fmap = R.fmap

instance Functor (W.WriterT w) where
  fmap = W.fmap

