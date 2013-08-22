
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
  module MHask.Join,
  module MHask.Duplicate,
  module MHask.Layered,
  module MHask.UnFunctor,

  -- * Indexed Classes
  module MHask.Indexed.Functor,
  module MHask.Indexed.Pointed,
  module MHask.Indexed.Monad,
  module MHask.Indexed.Copointed,
  module MHask.Indexed.Comonad,
  module MHask.Indexed.Join,
  module MHask.Indexed.Duplicate,
  module MHask.Indexed.Layered,
  module MHask.Indexed.UnFunctor,
  ) where

import MHask.Arrow

import MHask.Functor
import MHask.Pointed
import MHask.Monad
import MHask.Copointed
import MHask.Comonad
import MHask.Join
import MHask.Duplicate
import MHask.Layered
import MHask.UnFunctor


import MHask.Indexed.Functor
import MHask.Indexed.Pointed
import MHask.Indexed.Monad
import MHask.Indexed.Copointed
import MHask.Indexed.Comonad
import MHask.Indexed.Join
import MHask.Indexed.Duplicate
import MHask.Indexed.Layered
import MHask.Indexed.UnFunctor

