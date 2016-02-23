module Control.Eff.Base where

import Control.Eff

import Control.Monad
import Control.Monad.ST


data Base m x where
  Lift :: m a -> Base m a


lift :: (Base m :< eff) => m a -> Eff eff a
lift m = inject (Lift m)

-- | Unlike mtl 'lift' vs 'liftIO', this is actually just a synonym. There is
-- only one "layer" to lift to - the rest of the effect.
liftIO :: (Base IO :< eff) => IO a -> Eff eff a
liftIO = lift

liftST :: (Base (ST s) :< eff) => ST s a -> Eff eff a
liftST = lift


runBase :: Monad m => Eff (Base m :+ Void) a -> m a
runBase (Pure a) = pure a
runBase (Impure (InL (Lift m)) k) = m >>= runBase . k

runIO :: Eff (Base IO :+ Void) a -> IO a
runIO = runBase

runST_ :: (forall s. Eff (Base (ST s) :+ Void) a) -> a
runST_ m = runST (runBase m)
