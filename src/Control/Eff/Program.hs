module Control.Eff.Program where

import Control.Eff


data Program f x where
  Instr :: f a -> Program f a


instr :: (Program f :< effs) => f a -> Eff effs a
instr f = eta (Instr f)


runProgram :: Eff (Program f ': effs) a -> (forall x. f x -> Eff effs x) -> Eff effs a
runProgram eff phi = eliminate pure (\(Instr instr) k -> phi instr >>= k) eff
