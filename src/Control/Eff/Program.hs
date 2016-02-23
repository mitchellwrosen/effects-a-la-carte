module Control.Eff.Program where

import Control.Eff


data Program f x where
  Instr :: f a -> Program f a


instr :: (Program f :< eff) => f a -> Eff eff a
instr f = inject (Instr f)


runProgram :: Eff (Program f :+ eff) a -> (forall x. f x -> Eff eff x) -> Eff eff a
runProgram (Pure a) _ = pure a
runProgram (Impure (InL (Instr instr)) k) phi = phi instr >>= \x -> runProgram (k x) phi
runProgram (Impure (InR eff) k) phi = Impure eff (\x -> runProgram (k x) phi)
