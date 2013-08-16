{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to base.Prelude (Monad)
module MHask.Monad where

import Prelude hiding (Monad)
import qualified Prelude as P
import MHask.Arrow

import qualified MHask.Functor as MHask
import qualified MHask.Pointed as MHask


-- | Dual of "MHask.Comonad"
class (MHask.Pointed t) => Monad t where
  -- | Instances must satisfy the following laws:
  -- 
  -- > return ~>~ join ≡ identityArrow
  -- > fmap f ~>~ join ≡ join ~>~ f
  -- 
  -- The culmination of these laws
  -- allows us to state the following:
  -- 
  -- > return ~>~ fmap f ~>~ join ≡ f
  join :: (P.Monad m)
    => t (t m) ~> t m
  default join :: (P.Monad m, P.Monad (t m))
    => t (t m) ~> t m
  join = bind id

  bind :: (P.Monad m, P.Monad n)
    => (m ~> t n) -> (t m ~> t n)
  default bind ::
    (P.Monad m, P.Monad n,
     P.Monad (t m), P.Monad (t n),
     P.Monad (t (t n)))
    => (m ~> t n) -> (t m ~> t n)
  bind f = MHask.fmap f ~>~ join


-- | If you define your Monad in terms of bind and return,
-- then you get a free implementation of fmap which can
-- be used for Functor.
fmapMonad :: (P.Monad m, P.Monad n, P.Monad (t n), Monad t)
  => (m ~> n) -> (t m ~> t n)
fmapMonad f = bind (f ~>~ MHask.return)
