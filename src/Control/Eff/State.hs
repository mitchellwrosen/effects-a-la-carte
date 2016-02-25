{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Eff.State where

import Control.Eff
import Control.Eff.Reader
import Control.Eff.Writer


get :: (Reader r :< effs) => Eff effs r
get = ask

put :: (Writer s :< effs) => s -> Eff effs ()
put = tell

modify :: ([Reader s, Writer s] :<< effs) => (s -> s) -> Eff effs ()
modify f = get >>= put . f


runState :: Eff (Reader s ': Writer s ': effs) a -> s -> Eff effs (a, s)
runState (Pure a) s = pure (a, s)
runState (Impure es k) s =
  case decomp es of
    Left es' ->
      case decomp es' of
        Left es'' -> Impure es'' (\x -> runState (k x) s)
        Right (Tell s') -> runState (k ()) s'
    Right Ask -> runState (k s) s

evalState :: Eff (Reader s ': Writer s ': effs) a -> s -> Eff effs a
evalState f s = fst <$> runState f s

execState :: Eff (Reader s ': Writer s ': effs) a -> s -> Eff effs s
execState f s = snd <$> runState f s
