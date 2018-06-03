{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Exception where

import Control.Effect

data Exception exception result = Throw exception

throwError :: Member (Exception exception) effects => exception -> Effect effects a
throwError = send . Throw
