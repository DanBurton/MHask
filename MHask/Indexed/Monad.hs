{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to indexed.Control.Monad.Indexed (IxMonad)
module MHask.Indexed.Monad where



import MHask.Arrow

import qualified MHask.Indexed.Pointed as MHask
import qualified MHask.Indexed.Functor as MHask

-- | Indexed version of "MHask.Monad".
-- Dual of "MHask.Indexed.Comonad"
class (MHask.IxPointed t) => IxMonad t where
  -- | Instances must satisfy the following laws:
  -- 
  -- > imap ijoin    ~>~ ijoin ≡ ijoin ~>~ ijoin
  -- > imap (imap f) ~>~ ijoin ≡ ijoin ~>~ imap f
  -- 
  -- >      ireturn ~>~ ijoin ≡ identityArrow
  -- > imap ireturn ~>~ ijoin ≡ identityArrow
  ijoin :: (Monad m)
    => t i j (t j k m) ~> t i k m
  default ijoin :: (Monad m, Monad (t j k m))
    => t i j (t j k m) ~> t i k m
  ijoin = ibind id

  -- | Instances must satisfy the following laws
  -- 
  -- > ibind ireturn ≡ identityArrow
  -- > ireturn ~>~ ibind f ≡ f
  -- > ibind f ~>~ ibind g ≡ bind (f ~>~ bind g)
  ibind :: (Monad m, Monad n)
    => (m ~> t j k n) -> (t i j m ~> t i k n)
  default ibind ::
    (Monad m, Monad n,
     Monad (t i j m), Monad (t j k n), Monad (t i k n),
     Monad (t i j (t j k n)))
    => (m ~> t j k n) -> (t i j m ~> t i k n)
  ibind f = MHask.imap f ~>~ ijoin


-- | If you define your IxMonad in terms of ibind and ireturn,
-- then you get a free implementation of imap which can
-- be used for IxFunctor.
imapMonad :: (Monad m, Monad n, Monad (t j j n), IxMonad t)
  => (m ~> n) -> (t i j m ~> t i j n)
imapMonad f = ibind (f ~>~ MHask.ireturn)
