{-# LANGUAGE FunctionalDependencies #-}

module Control.Eff.Fresh where

import Control.Eff

import Control.Concurrent.Supply


data Fresh a x where
  Fresh :: Fresh a a


fresh :: (Fresh a :< eff) => Eff eff a
fresh = inject Fresh


runFresh :: IsSupply s a => Eff (Fresh a :+ eff) b -> s -> Eff eff b
runFresh (Pure a) _ = pure a
runFresh (Impure (InL Fresh) k) s =
  let (n, s') = fresh_ s
  in runFresh (k n) s'
runFresh (Impure (InR eff) k) s = Impure eff (\x -> runFresh (k x) s)


class IsSupply s a | s -> a where
  fresh_ :: s -> (a, s)

instance IsSupply Supply Int where
  fresh_ = freshId

instance IsSupply [a] a where
  fresh_ (x:xs) = (x,xs)
  fresh_ [] = error "IsSupply.fresh_: empty list"
