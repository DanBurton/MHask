{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Join where

import MHask.Arrow

import qualified MHask.Functor as MHask

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R
import qualified MHask.Impl.Writer   as W


-- | Dual of "MHask.Duplicate".
class (MHask.Functor t) => Join t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > fmap join     ~>~ join ≡ join ~>~ join
  -- > fmap (fmap f) ~>~ join ≡ join ~>~ fmap f
  join :: Monad m => t (t m) ~> t m


instance Join I.IdentityT where
  join = I.extract

instance (S.Monoid s) => Join (S.StateT s) where
  join = S.extract

instance (R.Monoid r) => Join (R.ReaderT r) where
  join = R.extract

-- Not sure why this requires Monoid. It shouldn't.
instance (W.Monoid w) => Join (W.WriterT w) where
  join = W.extract
