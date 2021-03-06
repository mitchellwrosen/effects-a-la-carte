{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Eff.Except where

import Control.Eff


data Except e x where
  Raise :: e -> Except e a


raise :: (Except e :< effs) => e -> Eff effs a
raise e = eta (Raise e)

catch :: (Except e :< effs) => Eff effs a -> (e -> Eff effs a) -> Eff effs a
catch eff k = interpose pure (\(Raise e) _ -> k e) eff


runExcept :: Eff (Except e ': effs) a -> Eff effs (Either e a)
runExcept = eliminate (pure . Right) (\(Raise e) _ -> pure (Left e))
