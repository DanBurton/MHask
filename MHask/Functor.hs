
-- | Compare to base.Prelude.Functor (Functor)
module MHask.Functor where

import Prelude hiding (Functor, fmap)
import MHask.Arrow

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer



-- | Functor is its own dual.
class Functor t where
  -- | Flipping the arrows on fmap's type signature
  -- is just the same type signature in disguise.
  -- 
  -- > (m <~ n) -> (t m <~ t n)
  fmap :: (Monad m, Monad n)
    => (m ~> n) -> (t m ~> t n)


instance Functor (StateT s) where
  fmap f m = StateT $ \s -> f (runStateT m s)

instance Functor (ReaderT r) where
  fmap f m = ReaderT $ \r -> f (runReaderT m r)

instance Functor (WriterT w) where
  fmap f m = WriterT $ f (runWriterT m)

