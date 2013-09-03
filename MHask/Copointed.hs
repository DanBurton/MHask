{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Copointed)
module MHask.Copointed where


import MHask.Arrow


import qualified MHask.Join as MHask


-- | Dual of "MHask.Pointed".
class (MHask.Join t) => Copointed t where
  -- | Instances must obey the following laws:
  -- 
  -- > join ≡ extract :: t m <~ t (t m)
  -- > extract ~<~ fmap f ≡ f ~<~ extract
  extract :: (Monad m)
    => m <~ t m

