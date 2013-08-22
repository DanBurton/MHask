{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | No comparison that I'm aware of.
module MHask.Indexed.UnFunctor where

import MHask.Arrow

import qualified MHask.Indexed.Pointed   as MHask
import qualified MHask.Indexed.Copointed as MHask

-- | IxUnFunctor is its own dual.
class (MHask.IxPointed t, MHask.IxCopointed t)
  => IxUnFunctor t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > ireturn ~>~ iextract ≡ identityArrow
  -- 
  -- Custom implementations should be equivalent to the
  -- default implementation
  -- 
  -- > unImap f ≡ ireturn ~>~ f ~>~ iextract
  -- 
  -- From all laws required so far, it follows that
  -- 
  -- > unImap (imap f) ≡ f
  -- > unImap identityArrow ≡ identityArrow
  -- > unImap (imap f ~>~ imap g) ≡ f ~>~ g
  -- 
  -- However, take note that the following are not guaranteed,
  -- and are usually not true:
  -- 
  -- > imap (unImap f)      ≟ f
  -- > iextract ~>~ ireturn ≟ identityArrow ∷ t i i m ~> t i i m
  -- > unImap (f ~>~ g)     ≟ unImap f ~>~ unImap g
  unImap :: (Monad m, Monad n)
    => (t i i m ~> t j j n) -> (m ~> n)
  default unImap
    :: (Monad m, Monad n,
        Monad (t i i m), Monad (t j j n))
    => (t i i m ~> t j j n) -> (m ~> n)
  unImap f = MHask.ireturn ~>~ f ~>~ MHask.iextract

