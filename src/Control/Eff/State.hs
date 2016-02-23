module Control.Eff.State
  ( State
  , get
  , put
  , modify
  , runState
  , evalState
  , execState
  ) where

import Control.Eff


data State s x where
  Modify :: (s -> s) -> State s s


get :: (State s :<: eff) => Eff eff s
get = _modify id

put :: (State s :<: eff) => s -> Eff eff ()
put s = modify (const s)

modify :: (State s :<: eff) => (s -> s) -> Eff eff ()
modify f = () <$ _modify f

_modify :: (State s :<: eff) => (s -> s) -> Eff eff s
_modify f = inject (Modify f)


runState :: Eff (State s :+: eff) a -> s -> Eff eff (a, s)
runState (Pure a) s = Pure (a, s)
runState (Impure (InL (Modify f)) k) s = runState (k s) (f s)
runState (Impure (InR eff) k) s = Impure eff (\x -> runState (k x) s)

evalState :: Eff (State s :+: eff) a -> s -> Eff eff a
evalState f s = fst <$> runState f s

execState :: Eff (State s :+: eff) a -> s -> Eff eff s
execState f s = snd <$> runState f s
