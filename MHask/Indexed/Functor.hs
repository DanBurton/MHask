
-- | Compare to indexed.Data.Functor.Indexed (IxFunctor)
module MHask.Indexed.Functor where

import MHask.Arrow

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Functor as MHask

-- | The indexed version of "MHask.Functor".
-- IxFunctor is its own dual.
class IxFunctor t where
  -- | Flipping the arrows on imap's type signature
  -- is just the same type signature in disguise.
  -- 
  -- > (m <~ n) -> (t i j m <~ t i j n)
  imap :: (Monad m, Monad n)
    => (m ~> n) -> (t i j m ~> t i j n)
  default imap :: (Monad m, Monad n,
                   MHask.Functor (t i j))
    => (m ~> n) -> (t i j m ~> t i j n)
  imap = MHask.fmap
