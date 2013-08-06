{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | This module sets the stage for the rest of the package.
-- It defines a type synonym @~>@ which cleans up the
-- type signatures,
-- and  @~>~@ which is used in the default implementation
-- of bind. These represent the type of arrows and arrow composition
-- in MHask, respectively.
-- 
-- By using @~>@, type signatures for the MHask class operations
-- can be easily compared to their Hask counterparts. However,
-- as a reminder that you are dealing with Monads, where
-- typically you would see @a@ and @b@ in the Hask counterpart,
-- you will instead see @m@ and @n@, and where you would
-- typically see @m@ or @w@, you will instead see @t@,
-- as a mnemonic for Monad transformer.
-- 
-- For illustrative purposes, this module also provides
-- @<~@ and @~<~@, to clearly illustrate how duals are
-- nothing more than just \"flipping the arrows\".
-- You are encouraged to compare docs or even source files
-- to see just how similar they are, all the way down
-- to default implementations.
module MHask.Arrow where

-- | The @~>@ type represents arrows in the
-- category of MHask. 
type m ~> n = forall x. m x -> n x

-- | It's just @~>@ flipped.
-- 
-- > type m <~ n = n ~> m
type m <~ n = n ~> m

-- | Left-to-right composition of arrows in MHask.
(~>~) :: (Monad a, Monad b, Monad c) => (a ~> b) -> (b ~> c) -> (a ~> c)
f1 ~>~ f2 = f2 . f1

-- | It's just @~>~@ flipped.
-- 
-- > (~<~) = flip (~>~)
(~<~) :: (Monad a, Monad b, Monad c) => (c <~ b) -> (b <~ a) -> (c <~ a)
(~<~) = flip (~>~)
