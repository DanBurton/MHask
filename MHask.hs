
-- | Various abstractions in the category of MHask.
-- This re-exports everything, and should be imported
-- qualified so that the operations and classes
-- do not clash with their Hask counterparts.
-- 
-- > import qualified MHask
module MHask (
  -- * Prelimiary
  module MHask.Arrow,

  -- * Classes
  module MHask.Functor,
  module MHask.Pointed,
  module MHask.Monad,
  module MHask.Copointed,
  module MHask.Comonad,

  -- * Indexed Classes
  module MHask.Indexed.Functor,
  module MHask.Indexed.Pointed,
  module MHask.Indexed.Monad,
  module MHask.Indexed.Copointed,
  module MHask.Indexed.Comonad,
  ) where

import MHask.Arrow

import MHask.Functor
import MHask.Pointed
import MHask.Monad
import MHask.Copointed
import MHask.Comonad

import MHask.Indexed.Functor
import MHask.Indexed.Pointed
import MHask.Indexed.Monad
import MHask.Indexed.Copointed
import MHask.Indexed.Comonad
