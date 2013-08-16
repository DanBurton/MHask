{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to indexed.Control.Comonad.Indexed (IxComonad)
module MHask.Indexed.Comonad where



import MHask.Arrow

import qualified MHask.Indexed.Functor as MHask
import qualified MHask.Indexed.Copointed as MHask

-- | Indexed version of "MHask.Comonad".
-- Dual of "MHask.Indexed.Monad"
class (MHask.IxCopointed t) => IxComonad t where
  -- | Instances must satisfy the following laws:
  -- 
  -- > iextend ~<~ iduplicate ≡ identityArrow
  -- > imap f  ~<~ iduplicate ≡ iduplicate ~<~ f
  -- 
  -- The culmination of these laws
  -- allows us to state the following:
  -- 
  -- > iextend ~<~ imap f ~<~ iduplicate ≡ f
  iduplicate :: (Monad m)
    => t i j (t j k m) <~ t i k m
  default iduplicate :: (Monad m, Monad (t j k m))
    => t i j (t j k m) <~ t i k m
  iduplicate = iextend id

  iextend :: (Monad m, Monad n)
    => (m <~ t j k n) -> (t i j m <~ t i k n)
  default iextend ::
    (Monad m, Monad n,
     Monad (t i j m), Monad (t j k n), Monad (t i k n),
     Monad (t i j (t j k n)))
    => (m <~ t j k n) -> (t i j m <~ t i k n)
  iextend f = MHask.imap f ~<~ iduplicate


-- | If you define your IxComonad in terms of iextend and iextract,
-- then you get a free implementation of imap which can
-- be used for IxFunctor.
imapComonad :: (Monad m, Monad n, Monad (t j j n), IxComonad t)
  => (m <~ n) -> (t i j m <~ t i j n)
imapComonad f = iextend (f ~<~ MHask.iextract)
