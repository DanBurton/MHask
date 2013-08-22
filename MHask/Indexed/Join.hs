{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Indexed.Join where

import MHask.Arrow

import qualified MHask.Indexed.Functor as MHask

-- | Indexed version of "MHask.Join".
-- Dual of "MHask.Indexed.Duplicate".
class (MHask.IxFunctor t) => IxJoin t where
  ijoin :: (Monad m) => t i j (t j k m) ~> t i k m

