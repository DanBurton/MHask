{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Copointed)
module MHask.Copointed where


import MHask.Arrow


import qualified MHask.Join as MHask

import qualified MHask.Impl.Identity as I
import qualified MHask.Impl.State    as S
import qualified MHask.Impl.Reader   as R
import qualified MHask.Impl.Writer   as W


-- | Dual of "MHask.Pointed".
class (MHask.Join t) => Copointed t where
  -- | Instances must obey the following laws:
  -- 
  -- > join ≡ extract :: t m <~ t (t m)
  -- > extract ~<~ fmap f ≡ f ~<~ extract
  extract :: (Monad m)
    => m <~ t m


instance Copointed I.IdentityT where
  extract = I.extract

instance (S.Monoid s) => Copointed (S.StateT s) where
  extract = S.extract

instance (R.Monoid r) => Copointed (R.ReaderT r) where
  extract = R.extract

instance (W.Monoid w) => Copointed (W.WriterT w) where
  extract = W.extract