{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Duplicate where

import MHask.Arrow

import qualified MHask.Functor as MHask

-- | Dual of "MHask.Join".
class (MHask.Functor t) => Duplicate t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > fmap duplicate ~<~ duplicate ≡ duplicate ~<~ duplicate
  -- > fmap (fmap f)  ~<~ duplicate ≡ duplciate ~<~ fmap f
  duplicate :: Monad m => t (t m) <~ t m

