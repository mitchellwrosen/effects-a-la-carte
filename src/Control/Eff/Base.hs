{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Eff.Base where

import Control.Eff

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST


data Base m x where
  Lift :: m a -> Base m a


lift :: (Base m :< effs) => m a -> Eff effs a
lift m = eta (Lift m)

-- | Unlike mtl 'lift' vs 'liftIO', this is actually just a synonym. There is
-- only one "layer" to lift to - the rest of the effect.
liftIO :: (MonadIO m, Base m :< eff) => m a -> Eff eff a
liftIO = lift

liftST :: (Base (ST s) :< eff) => ST s a -> Eff eff a
liftST = lift


runBase :: Monad m => Eff '[Base m] a -> m a
runBase (Pure a) = pure a
runBase (Impure es k) =
  case decomp es of
    Right (Lift m) -> m >>= \x -> runBase (k x)
    -- Left impossible

runIO :: MonadIO m => Eff '[Base m] a -> m a
runIO = runBase

runST_ :: (forall s. Eff '[Base (ST s)] a) -> a
runST_ m = runST (runBase m)
