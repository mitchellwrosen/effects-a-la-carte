module Control.Eff.State where

import Control.Eff

data State s x where
  Get :: State s s
  Put :: s -> State s ()

get :: (State s :<: eff) => Eff eff s
get = inject Get

put :: (State s :<: eff) => s -> Eff eff ()
put s = inject (Put s)

runState :: Eff (State s :+: eff) a -> s -> Eff eff (a, s)
runState (Pure a)                 s = Pure (a, s)
runState (Impure (InL Get)     k) s = runState (k s) s
runState (Impure (InL (Put s)) k) _ = runState (k ()) s
runState (Impure (InR eff)     k) s = Impure eff (\x -> runState (k x) s)
