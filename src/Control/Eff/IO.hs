module Control.Eff.IO where

import Control.Eff

data IO_ a = IO_ (IO a)

io :: (IO_ :<: eff) => IO a -> Eff eff a
io m = inject (IO_ m)

runIO :: Eff (IO_ :+: Void) a -> IO a
runIO (Pure a) = pure a
runIO (Impure (InL (IO_ m)) k) = m >>= \x -> runIO (k x)
