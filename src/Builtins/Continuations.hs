{-# LANGUAGE OverloadedStrings #-}

module Builtins.Continuations (builtinContinuations) where

import Builtins.Utils (builtinApp)
import Core (combine)
import Errors
import Types
import Control.Monad.Cont (callCC)
import Control.Monad.IO.Class (liftIO)

builtinContinuations :: [(Symbol, Expr r)]
builtinContinuations =
  [ ("call/cc", builtinApp primCallCC)
  , ("continuation->applicative", builtinApp continuationToApplicative)
  , ("extend-continuation", builtinApp extendContinuation)
  ]

primCallCC :: Builtin r
primCallCC env [x] = do
  c <- getCombiner "call/cc" x
  callCC $ \k -> combine env c [LContinuation k]
primCallCC _ args = numArgs "call/cc" 1 args

continuationToApplicative :: Builtin r
continuationToApplicative _ [x] = do
  c <- getContinuation "continuation->applicative" x
  pure $ builtinApp $ \_ exprs ->
    case exprs of
      -- TODO: operand tree
      [v] -> c v
      args -> numArgs "<continuation>" 1 args
continuationToApplicative _ args = numArgs "continuation->applicative" 1 args

extendContinuation :: Builtin r
extendContinuation _ [c, app, env] = do
  c' <- getContinuation "extend-continuation" c
  appCombiner <- getApplicative "extend-continuation" app
  env' <- getEnvironment "extend-continuation" env
  pure $ LContinuation $ \v -> do
    c' =<< combine env' appCombiner [v]
extendContinuation env [c, app] = do
  emptyEnv <- liftIO $ newEnvironment []
  extendContinuation env [c, app, LEnv emptyEnv]
extendContinuation _ args = numArgsBound "extend-continuation" (2, 3) args

