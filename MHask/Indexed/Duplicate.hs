{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Indexed.Duplicate where

import MHask.Arrow

import qualified MHask.Indexed.Functor as MHask

-- | Indexed version of "MHask.Duplicate".
-- Dual of "MHask.Indexed.Join".
class (MHask.IxFunctor t) => IxDuplicate t where
  iduplicate :: (Monad m) => t i j (t j k m) <~ t i k m

