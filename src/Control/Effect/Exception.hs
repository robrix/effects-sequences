{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Exception where

import Control.Effect

data Exception exception result = Throw exception

throwError :: Member (Exception exception) effects => exception -> Effect effects a
throwError = send . Throw

catchError :: Member (Exception exception) effects => Effect effects a -> (exception -> Effect effects a) -> Effect effects a
catchError action handler = interpose pure (\ (Throw exception) _ -> handler exception) action
