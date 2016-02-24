module Control.Eff.Select where

import Control.Eff


data Select x where
  Select :: [a] -> Select a


select :: (Select :< effs) => [a] -> Eff effs a
select xs = eta (Select xs)


runSelect :: Eff (Select ': effs) a -> Eff effs [a]
runSelect = eliminate (\a -> pure [a]) (\(Select xs) k -> concat <$> mapM k xs)
