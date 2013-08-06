{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | Compare to comonad.Control.Comonad (Copointed)
module MHask.Copointed where


import MHask.Arrow

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified MHask.Functor as MHask



-- | The dual of "MHask.Pointed"
class (MHask.Functor t) => Copointed t where
  extract :: (Monad m)
    => m <~ t m


instance (Monoid s) => Copointed (StateT s) where
  extract = flip evalStateT mempty

instance (Monoid r) => Copointed (ReaderT r) where
  extract = flip runReaderT mempty

instance Copointed (WriterT w) where
  extract = liftM fst . runWriterT

