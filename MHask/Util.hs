{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module MHask.Util where

type m ~> n = forall x. m x -> n x

(~>) :: (Monad a, Monad b, Monad c) => (a ~> b) -> (b ~> c) -> (a ~> c)
f1 ~> f2 = f2 . f1
