{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Copointed)
module MHask.Copointed where


import MHask.Arrow

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


import qualified MHask.Join as MHask


-- | Dual of "MHask.Pointed".
class (MHask.Join t) => Copointed t where
  -- | Instances must obey the following laws:
  -- 
  -- > join === extract :: t m <~ t (t m)
  -- > extract ~<~ fmap f ≡ f ~<~ extract
  extract :: (Monad m)
    => m <~ t m





{-
instance (Monoid s) => Copointed (StateT s) where
  extract = flip evalStateT mempty

instance (Monoid r) => Copointed (ReaderT r) where
  extract = flip runReaderT mempty

instance Copointed (WriterT w) where
  extract = liftM fst . runWriterT
-}

