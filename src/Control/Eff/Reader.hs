module Control.Eff.Reader where

import Control.Eff

data Reader r x where
  Ask :: Reader r r

ask :: (Reader r :< eff) => Eff eff r
ask = inject Ask

runReader :: Eff (Reader r :+ eff) a -> r -> Eff eff a
runReader (Pure a)             _ = Pure a
runReader (Impure (InL Ask) k) r = runReader (k r) r
runReader (Impure (InR eff) k) r = Impure eff (\x -> runReader (k x) r)
