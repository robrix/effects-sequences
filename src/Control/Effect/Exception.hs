{-# LANGUAGE ExistentialQuantification, FlexibleContexts, TypeOperators #-}
module Control.Effect.Exception where

import Control.Effect

data Exception exception m result
  = Throw exception
  | forall x . Catch (m x) (exception -> m x) (x -> m result)

throwError :: Member (Exception exception) effects => exception -> Eff effects a
throwError = send . Throw

catchError :: Member (Exception exception) effects => Eff effects a -> (exception -> Eff effects a) -> Eff effects a
catchError action handler = send (Catch action handler pure)
-- catchError action handler = interpose pure (\ (Throw exception) _ -> handler exception) action

runException :: (effects \\ S (Exception exception)) rest => Eff effects a -> Eff rest (Either exception a)
runException = relayEffect (pure . Right) (\ (Throw exception) _ -> pure (Left exception))
