{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Eff.Fresh where

import Control.Eff

import Control.Concurrent.Supply


data Fresh a x where
  Fresh :: Fresh a a


fresh :: (Fresh a :< effs) => Eff effs a
fresh = eta Fresh


runFresh :: IsSupply s a => Eff (Fresh a ': effs) b -> s -> Eff effs b
runFresh (Pure b) _ = pure b
runFresh (Impure es k) s =
  case decomp es of
    Left es' -> Impure es' (\x -> runFresh (k x) s)
    Right Fresh ->
      let (a, s') = fresh_ s
      in runFresh (k a) s'


class IsSupply s a | s -> a where
  fresh_ :: s -> (a, s)

instance IsSupply Supply Int where
  fresh_ = freshId

instance IsSupply [a] a where
  fresh_ (x:xs) = (x,xs)
  fresh_ [] = error "IsSupply.fresh_: empty list"
