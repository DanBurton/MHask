{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to base.Prelude (Monad)
module MHask.Monad where

import Prelude hiding (Monad)
import qualified Prelude as P
import MHask.Arrow

import qualified MHask.Functor as MHask
import qualified MHask.Join    as MHask
import qualified MHask.Layered as MHask
import qualified MHask.Pointed as MHask


-- | Dual of "MHask.Comonad"
-- 
-- The various prerequisite laws of this class
-- allow us to state the following law:
-- 
-- > return ~>~ fmap f ~>~ join ≡ f
class (MHask.Layered t, MHask.Pointed t) => Monad t where
  -- | Instances must satisfy the following laws:
  -- 
  -- > bind return ≡ identityArrow
  -- > return ~>~ bind f ≡ f
  -- > bind f ~>~ bind g ≡ bind (f ~>~ bind g)
  bind :: (P.Monad m, P.Monad n)
    => (m ~> t n) -> (t m ~> t n)
  default bind ::
    (P.Monad m, P.Monad n,
     P.Monad (t m), P.Monad (t n),
     P.Monad (t (t n)))
    => (m ~> t n) -> (t m ~> t n)
  bind f = MHask.fmap f ~>~ MHask.join


-- | If you define your Monad in terms of bind and return,
-- then you get a free implementation of fmap.
fmapMonad :: (P.Monad m, P.Monad n, P.Monad (t n), Monad t)
  => (m ~> n) -> (t m ~> t n)
fmapMonad f = bind (f ~>~ MHask.return)

-- | If you define your Monad in terms of @bind@ and @return@,
-- then you can get a free implementation of @join@.
joinMonad :: (P.Monad m, P.Monad (t m), Monad t)
  => t (t m) ~> t m
joinMonad = bind identityArrow

