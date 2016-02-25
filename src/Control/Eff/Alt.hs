{-# LANGUAGE UndecidableInstances #-}

module Control.Eff.Alt where

import Control.Eff

import Control.Applicative
import Control.Monad


data Alt x where
  Empty :: Alt x
  Alt   :: Alt Bool

instance (Alt :< effs) => Alternative (Eff effs) where
  empty = eta Empty
  m1 <|> m2 = eta Alt >>= \b -> if b then m1 else m2

instance (Alt :< effs) => MonadPlus (Eff effs) where
  mzero = empty
  mplus = (<|>)


runAlt :: Alternative f => Eff (Alt ': effs) a -> Eff effs (f a)
runAlt = eliminate (pure . pure) (\Alt k -> (<|>) <$> k True <*> k False)
