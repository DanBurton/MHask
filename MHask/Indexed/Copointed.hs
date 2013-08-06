{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Compare to indexed.Data.Functor.Indexed (IxCopointed)
module MHask.Indexed.Copointed where


import MHask.Util







import qualified MHask.Copointed as MHask
import qualified MHask.Indexed.Functor as MHask

-- | The indexed version of "MHask.Copointed".
-- The dual of "MHask.Indexed.Pointed".
class (MHask.IxFunctor t) => IxCopointed t where
  iextract :: (Monad m)
    => m <~ t i i m
  default iextract :: (Monad m,
                       MHask.Copointed (t i i))
    => m <~ t i i m
  iextract = MHask.extract
