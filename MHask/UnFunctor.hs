{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | No comparison that I'm aware of.
module MHask.UnFunctor where

import MHask.Arrow

import qualified MHask.Pointed   as MHask
import qualified MHask.Copointed as MHask
import qualified MHask.Monad     as MHask
import qualified MHask.Comonad   as MHask
import qualified MHask.Layered   as MHask

-- | UnFunctor is its own dual.
-- 
-- The @unFmap@ operation is a generalization of @withLayer@,
-- and is the inverse of @fmap@.
class (MHask.Layered t, MHask.Monad t, MHask.Comonad t)
  => UnFunctor t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > return ~>~ extract ≡ identityArrow
  -- > withLayer ≡ unFmap ∷  (t (t m) ~> t (t n)) -> (t m ~> t n)
  -- 
  -- Custom implementations should be equivalent to the
  -- default implementation
  -- 
  -- > unFmap f ≡ return ~>~ f ~>~ extract
  -- 
  -- From all laws required so far, it follows that
  -- 
  -- > unFmap (fmap f) ≡ f
  -- > unFmap identityArrow ≡ identityArrow
  -- > unFmap (fmap f ~>~ fmap g) ≡ f ~>~ g
  -- 
  -- However, take note that the following are not guaranteed,
  -- and are usually not true:
  -- 
  -- > fmap (unFmap f)    ≟ f
  -- > extract ~>~ return ≟ identityArrow ∷ t m ~> t m
  -- > unFmap (f ~>~ g)   ≟ unFmap f ~>~ unFmap g
  unFmap :: (Monad m, Monad n)
    => (t m ~> t n) -> (m ~> n)
  default unFmap
    :: (Monad m, Monad n,
        Monad (t m), Monad (t n))
    => (t m ~> t n) -> (m ~> n)
  unFmap f = MHask.return ~>~ f ~>~ MHask.extract
