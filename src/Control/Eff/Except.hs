module Control.Eff.Except where

import Control.Eff

data Except e x where
  Raise :: e -> Except e a

raise :: (Except e :< eff) => e -> Eff eff a
raise e = inject (Raise e)

runExcept :: Eff (Except e :+ eff) a -> Eff eff (Either e a)
runExcept (Pure a) = Pure (Right a)
runExcept (Impure (InL (Raise e)) k) = Pure (Left e)
runExcept (Impure (InR eff) k) = Impure eff (\x -> runExcept (k x))
