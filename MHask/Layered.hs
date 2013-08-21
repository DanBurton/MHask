{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | No comparison that I'm aware of.
module MHask.Layered where

import MHask.Arrow

import qualified MHask.Join      as MHask
import qualified MHask.Duplicate as MHask

-- | Layered is its own dual.
class (MHask.Join t, MHask.Duplicate t) => Layered t where
  -- | Any instances must satisfy the following law:
  -- 
  -- > duplicate ~>~ join ≡ identityArrow ∷ t m ~> t m
  -- 
  -- Custom implementations should be equivalent to the
  -- default implementation
  -- 
  -- withLayer f ≡ duplicate ~>~ f ~>~ join
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


