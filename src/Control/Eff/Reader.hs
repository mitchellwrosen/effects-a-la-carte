module Control.Eff.Reader where

import Control.Eff


data Reader r x where
  Ask :: Reader r r


ask :: (Reader r :< effs) => Eff effs r
ask = eta Ask


runReader :: Eff (Reader r ': effs) a -> r -> Eff effs a
runReader eff r = eliminate pure (\Ask k -> k r) eff
