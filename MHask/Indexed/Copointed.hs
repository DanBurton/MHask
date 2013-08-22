{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to indexed.Data.Functor.Indexed (IxCopointed)
module MHask.Indexed.Copointed where


import MHask.Arrow







import qualified MHask.Copointed       as MHask
import qualified MHask.Indexed.Functor as MHask

-- | The indexed version of "MHask.Copointed".
-- The dual of "MHask.Indexed.Pointed".
class (MHask.IxFunctor t) => IxCopointed t where
  -- | Instances must obey the following law:
  -- 
  -- > iextract ~<~ imap f ≡ f ~<~ iextract
  iextract :: (Monad m)
    => m <~ t i i m
  default iextract :: (Monad m,
                       MHask.Copointed (t i i))
    => m <~ t i i m
  iextract = MHask.extract
