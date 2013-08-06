{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Compare to indexed.Data.Functor.Indexed (IxPointed)
module MHask.Indexed.Pointed where


import MHask.Util







import qualified MHask.Pointed as MHask
import qualified MHask.Indexed.Functor as MHask

-- | The indexed version of "MHask.Pointed".
-- The dual of "MHask.Indexed.Copointed".
class (MHask.IxFunctor t) => IxPointed t where
  ireturn :: (Monad m)
    => m ~> t i i m
  default ireturn :: (Monad m,
                      MHask.Pointed (t i i))
    => m ~> t i i m
  ireturn = MHask.return
