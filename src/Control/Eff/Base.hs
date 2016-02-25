{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Eff.Base where

import Control.Eff

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST

import qualified Control.Exception as E


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

-- | Catch asynchronous exceptions.
catchAsync
  :: forall a e effs.
     (Base IO :< effs, E.Exception e)
  => Eff effs a
  -> (e -> Eff effs a)
  -> Eff effs a
catchAsync eff handler = interpose pure imp eff
 where
  imp :: Base IO x -> (x -> Eff effs a) -> Eff effs a
  imp (Lift m) k = lift (E.try m) >>= \case
    Left e -> handler e
    Right x -> k x


runBase :: Monad m => Eff '[Base m] a -> m a
runBase (Pure a) = pure a
runBase (Impure es k) =
  case prj es of
    Just (Lift m) -> m >>= \x -> runBase (k x)
    -- Nothing impossible

runIO :: MonadIO m => Eff '[Base m] a -> m a
runIO = runBase

runST_ :: (forall s. Eff '[Base (ST s)] a) -> a
runST_ m = runST (runBase m)
