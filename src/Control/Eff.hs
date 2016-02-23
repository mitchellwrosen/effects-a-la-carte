module Control.Eff
  ( module Control.Eff
  , module Control.Eff.Void
  ) where

import Control.Eff.Void

import Control.Monad

data (f :+ g) a = InL (f a) | InR (g a)
infixr 5 :+


class f :< g where
  inj :: f a -> g a

instance f :< f where
  inj = id

instance {-# OVERLAPPING #-} f :< (f :+ g) where
  inj = InL

instance {-# OVERLAPPING #-} (f :< g) => f :< (h :+ g) where
  inj = InR . inj


data Eff eff a where
  Pure   :: a -> Eff eff a
  Impure :: eff x -> (x -> Eff eff a) -> Eff eff a

instance Functor (Eff eff) where
  fmap :: (a -> b) -> Eff eff a -> Eff eff b
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure g k) = Impure g (fmap f . k)

instance Applicative (Eff eff) where
  pure :: a -> Eff eff a
  pure = Pure

  (<*>) :: Eff eff (a -> b) -> Eff eff a -> Eff eff b
  Pure f <*> a = f <$> a
  Impure g k <*> a = Impure g (\x -> k x <*> a)

instance Monad (Eff eff) where
  return :: a -> Eff eff a
  return = Pure

  (>>=) :: Eff eff a -> (a -> Eff eff b) -> Eff eff b
  Pure a >>= f = f a
  Impure g k >>= f = Impure g (k >=> f)


run :: Eff Void a -> a
run (Pure a) = a

eta :: eff a -> Eff eff a
eta f = Impure f Pure

hoist :: (forall x. f x -> g x) -> Eff f a -> Eff g a
hoist _ (Pure a) = Pure a
hoist f (Impure g k) = Impure (f g) (\x -> hoist f (k x))

inject :: (f :< g) => f a -> Eff g a
inject = hoist inj . eta
