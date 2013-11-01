{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Layered where

import MHask.Arrow



import qualified MHask.Join      as MHask
import qualified MHask.Duplicate as MHask

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R
import qualified MHask.Impl.Writer   as W


-- | Layered is its own dual.



class (MHask.Join t, MHask.Duplicate t)
  => Layered t where
  -- | Any instances must satisfy the following law:
  -- 
  -- > duplicate ~>~ join ≡ identityArrow ∷ t m ~> t m
  -- 
  -- 
  -- Custom implementations should be equivalent to the
  -- default implementation
  -- 
  -- > withLayer f ≡ duplicate ~>~ f ~>~ join
  -- 
  -- From all laws required so far, it follows that:
  -- 
  -- > withLayer (fmap (fmap f))  ≡ fmap f
  -- > withLayer (fmap join)      ≡ join
  -- > withLayer (fmap duplicate) ≡ duplicate
  -- > withLayer identityArrow    ≡ identityArrow
  -- 
  -- However, take note that the following are not guaranteed,
  -- and are usually not true:
  -- 
  -- > join ~>~ duplicate  ≟ identityArrow ∷ t (t m) ~> t (t m)
  -- > withLayer (f ~>~ g) ≟ withLayer f ~>~ withLayer g
  withLayer :: (Monad m, Monad n)
    => (t (t m) ~> t (t n))
    -> (   t m  ~>    t n )
  default withLayer
    :: (Monad m, Monad n,
        Monad (t m), Monad (t n),
        Monad (t (t m)), Monad (t (t n)))
    => (t (t m) ~> t (t n))
    -> (   t m  ~>    t n )
  withLayer f = MHask.duplicate ~>~ f ~>~ MHask.join


instance Layered I.IdentityT
instance (S.Monoid s) => Layered (S.StateT s)
instance (R.Monoid r) => Layered (R.ReaderT r)
instance (W.Monoid w) => Layered (W.WriterT w)
