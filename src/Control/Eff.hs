module Control.Eff
  ( module Control.Eff
  , module Data.Union
  ) where

import Data.Union

import Control.Monad


data Eff (effs :: [* -> *]) (a :: *) where
  Pure   :: a -> Eff effs a
  Impure :: Union effs x -> (x -> Eff effs a) -> Eff effs a

instance Functor (Eff effs) where
  fmap :: (a -> b) -> Eff effs a -> Eff effs b
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure es k) = Impure es (fmap f . k)

instance Applicative (Eff effs) where
  pure :: a -> Eff effs a
  pure = Pure

  (<*>) :: Eff effs (a -> b) -> Eff effs a -> Eff effs b
  Pure f <*> a = f <$> a
  Impure es k <*> a = Impure es (\x -> k x <*> a)

instance Monad (Eff effs) where
  return :: a -> Eff effs a
  return = Pure

  (>>=) :: Eff effs a -> (a -> Eff effs b) -> Eff effs b
  Pure a >>= f = f a
  Impure es k >>= f = Impure es (k >=> f)


run :: Eff '[] a -> a
run (Pure a) = a

eta :: (eff :< effs) => eff a -> Eff effs a
eta f = Impure (inj f) Pure

-- | Handle an effect with continuations for the pure and impure cases.
eliminate
  :: forall a b eff effs.
     (a -> Eff effs b)
  -> (forall x. eff x -> (x -> Eff effs b) -> Eff effs b)
  -> Eff (eff ': effs) a
  -> Eff effs b
eliminate pur imp (Pure a) = pur a
eliminate pur imp (Impure es k) =
  case decomp es of
    Left es' -> Impure es' q
    Right e  -> imp e q
 where
  -- q :: x -> Eff effs b
  q x = eliminate pur imp (k x)
