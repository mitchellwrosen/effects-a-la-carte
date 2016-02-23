module Control.Eff.IO where

import Control.Eff

import Control.Monad.IO.Class


data IO_ a = IO_ (IO a)


io :: (IO_ :< eff) => IO a -> Eff eff a
io m = inject (IO_ m)


runIO :: MonadIO m => Eff (IO_ :+ Void) a -> m a
runIO (Pure a) = pure a
runIO (Impure (InL (IO_ m)) k) = liftIO m >>= \x -> runIO (k x)
