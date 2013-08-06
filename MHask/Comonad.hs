{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Comonad)
module MHask.Comonad where



import MHask.Util

import qualified MHask.Functor as MHask
import qualified MHask.Copointed as MHask


-- | Dual of "MHask.Monad"
class (MHask.Copointed t) => Comonad t where
  duplicate :: (Monad m)
    => t (t m) <~ t m
  default duplicate :: (Monad m, Monad (t m))
    => t (t m) <~ t m
  duplicate = extend id

  extend :: (Monad m, Monad n)
    => (m <~ t n) -> (t m <~ t n)
  default extend ::
    (Monad m, Monad n,
     Monad (t m), Monad (t n),
     Monad (t (t n)))
    => (m <~ t n) -> (t m <~ t n)
  extend f = MHask.fmap f ~<~ duplicate

