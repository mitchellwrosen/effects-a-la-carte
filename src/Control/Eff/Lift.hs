module Control.Eff.Lift where

import Control.Eff

import Control.Monad


data Lift m x where
  Lift :: m a -> Lift m a


lift :: (Lift m :< eff) => m a -> Eff eff a
lift m = inject (Lift m)


runLift :: Monad m => Eff (Lift m :+ Void) a -> m a
runLift (Pure a) = pure a
runLift (Impure (InL (Lift m)) k) = m >>= runLift . k
