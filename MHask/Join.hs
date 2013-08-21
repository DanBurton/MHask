{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Join where

import MHask.Arrow

import qualified MHask.Functor as MHask

-- Dual of "MHask.Duplicate".
class (MHask.Functor t) => Join t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > fmap join     ~>~ join ≡ join ~>~ join
  -- > fmap (fmap f) ~>~ join ≡ join ~>~ fmap f
  join :: Monad m => t (t m) ~> t m

