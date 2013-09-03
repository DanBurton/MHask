{-# LANGUAGE RankNTypes, TypeOperators, DefaultSignatures #-}

-- | This module sets the stage for the rest of the package.
-- It defines a type synonym @~>@ which cleans up the
-- type signatures,
-- and  @~>~@ which is used in default implementations.
-- These represent the type of arrows and arrow composition
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


-- | The @~>@ type is the type of arrows in the
-- category of MHask. However, it does not include
-- Monad constraints on m and n. These constraints should
-- instead appear at the head of any type signature that
-- uses the @~>@ type.
type m ~> n = forall x. m x -> n x


-- | It's just @~>@ flipped.
-- 
-- > type m <~ n = n ~> m
type m <~ n = n ~> m


-- | Left-to-right composition of arrows in MHask.
-- 
-- This satisfies the category laws for arrow composition in MHask.
-- In other words, arrow composition in MHask is associative.
-- 
-- > f ~>~ (g ~>~ h) ≡ (f ~>~ g) ~>~ h
(~>~) :: (Monad m, Monad m', Monad n) => (m ~> m') -> (m' ~> n) -> (m ~> n)
f1 ~>~ f2 = f2 . f1
{-# INLINE (~>~) #-}


-- | It's just @~>~@ flipped.
-- Notice how its type signature is the same as @~>~@,
-- just with the arrows flipped.
-- 
-- > (~<~) = flip (~>~)
(~<~) :: (Monad m, Monad m', Monad n) => (m <~ m') -> (m' <~ n) -> (m <~ n)
(~<~) = flip (~>~)
{-# INLINE (~<~) #-}


-- | The `identityArrow` is a polymorphic value which
-- specializes to the identity arrow for any given object @m@ in MHask.
-- 
-- > identityArrow = id
-- 
-- This satisfies the category laws for identity arrows in MHask.
-- In other words, it serves as both the left- and right-identity
-- of arrow composition in MHask.
-- 
-- > identityArrow ~>~ x ≡ x
-- > x ~>~ identityArrow ≡ x
identityArrow :: (Monad m) => m ~> m
identityArrow = id
{-# INLINE identityArrow #-}

