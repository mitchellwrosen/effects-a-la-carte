module Control.Eff.Writer where

import Control.Eff

import Data.Monoid


data Writer w x where
  Tell :: w -> Writer w ()


tell :: (Writer w :< effs) => w -> Eff effs ()
tell w = eta (Tell w)


runWriter
  :: Monoid w
  => Eff (Writer w ': effs) a
  -> Eff effs (a, w)
runWriter =
  eliminate
    (\a -> pure (a, mempty))
    (\(Tell w) k -> (\(a,w') -> (a, w <> w')) <$> k ())

runWriterWith
  :: (w -> Eff effs ())
  -> Eff (Writer w ': effs) a
  -> Eff effs a
runWriterWith f = eliminate pure (\(Tell w) k -> f w >> k ())

execWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs w
execWriter = fmap snd . runWriter
