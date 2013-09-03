{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | No comparison that I'm aware of
module MHask.Indexed.Layered where

import MHask.Arrow

import qualified MHask.Indexed.Join      as MHask
import qualified MHask.Indexed.Duplicate as MHask

-- | IxLayered is its own dual.
class (MHask.IxJoin t, MHask.IxDuplicate t)
  => IxLayered t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > iduplicate ~>~ ijoin ≡ identityArrow ∷ t i j m -> t i j m
  -- 
  -- Custom implementations should be equivalent to the
  -- default implementation
  -- 
  -- iwithLayer f ≡ iduplicate ~>~ f ~>~ ijoin
  -- 
  -- From all laws required so far, it follows that:
  -- 
  -- > iwithLayer (imap (imap f))   ≡ imap f
  -- > iwithLayer (imap ijoin)      ≡ join
  -- > iwithLayer (imap iduplicate) ≡ duplicate
  -- > iwithLayer identityArrow     ≡ identityArrow
  -- 
  -- However, take note that the following are not guaranteed,
  -- and are usually not true:
  -- 
  -- > ijoin ~>~ iduplicate  ≟ identityArrow
  -- > iwithLayer (f ~>~ g) ≟ iwithLayer f ~>~ iwithLayer g
  iwithLayer :: (Monad m, Monad n)
    => (t i j (t j k m) ~> t i' j' (t j' k' n))
    -> (t i        k m  ~> t i'          k' n )
  default iwithLayer
    :: (Monad m, Monad n,
        Monad (t i k m), Monad (t i' k' n),
        Monad (t i j (t j k m)), Monad (t i' j' (t j' k' n)))
    => (t i j (t j k m) ~> t i' j' (t j' k' n))
    -> (t i        k m  ~> t i'          k' n )
  iwithLayer f = MHask.iduplicate ~>~ f ~>~ MHask.ijoin

