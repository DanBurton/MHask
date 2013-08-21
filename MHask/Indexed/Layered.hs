{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | No comparison that I'm aware of
module MHask.Indexed.Layered where

import MHask.Arrow

import qualified MHask.Indexed.Join      as MHask
import qualified MHask.Indexed.Duplicate as MHask

-- | That which is both IxJoin and IxDuplicate is IxLayered
class (MHask.IxJoin t, MHask.IxDuplicate t) => IxLayered t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > iduplicate ~>~ ijoin ≡ identityArrow ∷ t i j m -> t i j m
  -- 
  -- > iwithLayer (imap f) ≡ f ∷ t i j m ~> t i k n
  iwithLayer
    :: (t i j (t j k m) ~> t i' j' (t j' k' n))
    -> (t i        k m  ~> t i'          k' n )
  iwithLayer f = MHask.iduplicate ~>~ f ~>~ MHask.ijoin
