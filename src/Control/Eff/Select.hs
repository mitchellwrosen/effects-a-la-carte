module Control.Eff.Select where

import Control.Eff


data Select x where
  Select :: [a] -> Select a


select :: (Select :<: eff) => [a] -> Eff eff a
select xs = inject (Select xs)


runSelect :: Eff (Select :+: eff) a -> Eff eff [a]
runSelect (Pure a) = pure [a]
runSelect (Impure (InL (Select xs)) k) = concat <$> mapM (runSelect . k) xs
runSelect (Impure (InR eff) k) = Impure eff (runSelect . k)
