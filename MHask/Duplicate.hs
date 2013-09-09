{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

module MHask.Duplicate where

import Control.Monad.Trans.Class (MonadTrans, lift)
import MHask.Arrow

import qualified MHask.Functor as MHask

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R

-- | Dual of "MHask.Join".
class (MHask.Functor t) => Duplicate t where
  -- | Any instances must satisfy the following laws:
  -- 
  -- > fmap duplicate ~<~ duplicate ≡ duplicate ~<~ duplicate
  -- > fmap (fmap f)  ~<~ duplicate ≡ duplciate ~<~ fmap f
  duplicate :: Monad m => t (t m) <~ t m
  default duplicate :: (Monad m, Monad (t m), MonadTrans t)
    => t (t m) <~ t m
  duplicate = lift


instance Duplicate I.IdentityT
instance Duplicate (S.StateT s)
instance Duplicate (R.ReaderT r)
