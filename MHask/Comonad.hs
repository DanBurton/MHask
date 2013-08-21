{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Comonad)
module MHask.Comonad where



import MHask.Arrow

import qualified MHask.Functor   as MHask
import qualified MHask.Duplicate as MHask
import qualified MHask.Layered   as MHask
import qualified MHask.Copointed as MHask


-- | Dual of "MHask.Monad".
-- 
-- The various prerequisite laws of this class
-- allow us to state the following law:
-- 
-- > extract ~<~ fmap f ~<~ duplicate ≡ f
class (MHask.Layered t, MHask.Copointed t) => Comonad t where
  -- | Laws, laws, everywhere. TODO
  extend :: (Monad m, Monad n)
    => (m <~ t n) -> (t m <~ t n)
  default extend ::
    (Monad m, Monad n,
     Monad (t m), Monad (t n),
     Monad (t (t n)))
    => (m <~ t n) -> (t m <~ t n)
  extend f = MHask.fmap f ~<~ MHask.duplicate


-- | If you define your Comonad in terms of extend and extract,
-- then you get a free implementation of fmap which can
-- be used for Functor.
fmapComonad :: (Monad m, Monad n, Monad (t n), Comonad t)
  => (m <~ n) -> (t m <~ t n)
fmapComonad f = extend (f ~<~ MHask.extract)

-- | If you define your Comonad in terms of extend and extract,
-- then you can get a free implementation of duplicate which can
-- be used for Duplicate
duplicateComonad :: (Monad m, Monad (t m), Comonad t)
  => t (t m) <~ t m
duplicateComonad = extend identityArrow

