{-# LANGUAGE ExistentialQuantification, FlexibleContexts, PolyKinds, TypeOperators #-}
module Control.Effect.Exception where

import Control.Effect
import Control.Effect.Internal
import Data.Effect.Union

data Throw exception m result
  = Throw exception

data Catch exception m result
  = forall x . Catch (m x) (exception -> m x) (x -> m result)

throwError :: Member (Throw exception) effects => exception -> Eff effects scopes a
throwError = send . Throw

catchError :: Member (Catch exception) scopes => Eff effects scopes a -> (exception -> Eff effects scopes a) -> Eff effects scopes a
catchError action handler = scope (Catch action handler pure)
-- catchError action handler = interpose pure (\ (Throw exception) _ -> handler exception) action

-- runException :: (effects \\ S (Exception exception)) rest => Eff effects a -> Eff rest (Either exception a)
-- runException = relayEffect (pure . Right) (\ (Throw exception) _ -> pure (Left exception))

runException :: ((effects \\ S (Throw exception)) effects', (scopes \\ S (Catch exception)) scopes', Scope (Union scopes')) => Eff effects scopes a -> Eff effects' scopes' (Either exception a)
runException (Pure a) = pure (Right a)
runException (Eff u q) = case strengthenSingleton <$> delete u of
  Left u' -> Eff u' (unit (Arrow (runException . dequeue q)))
  Right (Throw exc) -> pure (Left exc)
runException (Scope u) = case strengthenSingleton <$> delete u of
  Left u' -> Scope (handle (Right ()) (either (return . Left) runException) u')
  -- where hdl :: Either exception (Eff effects scopes x) -> Eff effects' scopes' (Either exception x)
  --       hdl = either (return . Left) runException
  Right (Catch action handler k) -> do
    action' <- runException action
    case action' of
      Left exc -> do
        handled <- runException (handler exc)
        case handled of
          Left exc -> pure (Left exc)
          Right a -> runException (k a)
      Right a -> runException (k a)
-- (∀x . Either e (Prog (HExc e+sig) x) → Prog sig (Either e x))
