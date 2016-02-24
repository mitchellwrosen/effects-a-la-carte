module Control.Eff.Writer where

import Control.Eff

import Data.Monoid


data Writer w x where
  Tell :: w -> Writer w ()


tell :: (Writer w :< eff) => w -> Eff eff ()
tell w = inject (Tell w)


runWriter :: forall a w eff. Monoid w => Eff (Writer w :+ eff) a -> Eff eff (a, w)
runWriter = go mempty
  where
    go :: w -> Eff (Writer w :+ eff) a -> Eff eff (a, w)
    go !acc (Pure a) = pure (a, acc)
    go !acc (Impure (InL (Tell w)) k) = go (acc <> w) (k ())
    go !acc (Impure (InR eff) k) = Impure eff (\x -> go acc (k x))

runWriterWith :: (w -> Eff eff ()) -> Eff (Writer w :+ eff) a -> Eff eff a
runWriterWith _ (Pure a) = pure a
runWriterWith f (Impure (InL (Tell w)) k) = f w >> runWriterWith f (k ())
runWriterWith f (Impure (InR eff) k) = Impure eff (\x -> runWriterWith f (k x))

execWriter :: Monoid w => Eff (Writer w :+ eff) a -> Eff eff w
execWriter = fmap snd . runWriter
