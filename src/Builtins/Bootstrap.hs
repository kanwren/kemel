{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Unique (newUnique)
import TextShow (showt)

import Builtins.Utils (builtinOp, builtinApp, allM)
import Core (eval, parseParamTree, matchParams, lookupVar, progn, defineVar)
import Errors
import Types

builtinBootstrap :: [(Symbol, Expr r)]
builtinBootstrap =
  [ ("$vau", builtinOp vau)
  , ("$define!", builtinOp define)
  , ("eval", builtinApp primEval)
  , ("wrap", builtinApp primWrap)
  , ("unwrap", builtinApp primUnwrap)
  , ("$if", builtinOp primIf)
  , ("$sequence", builtinOp progn)
  , ("make-environment", builtinApp makeEnvironment)
  , ("make-encapsulation-type", builtinApp makeEncapsulation)
  , ("cons", builtinApp cons)
  , ("$binds?", builtinOp binds)
  ]

-- NOTE: this is later redefined in the prelude to be able to compensate for
-- automatically $sequencing bodies
vau :: Builtin r
vau staticEnv [params, env, body] = do
  envName <- case env of
    LIgnore   -> pure Nothing
    LSymbol s -> pure $ Just s
    x -> evalError $ "$vau: invalid environment name: " <> showt x
  paramTree <- parseParamTree "$vau" params
  let
    closure = Closure
      { closureParams = paramTree
      , closureBody = body
      , closureStaticEnv = staticEnv
      , closureDynamicEnv = envName
      }
  pure $ LCombiner $ OperativeCombiner $ UserOp closure
vau _ args = numArgs "$vau" 3 args

define :: Builtin r
define env [bs, ps] = LInert <$ do
  tree <- parseParamTree "$define!" bs
  ps' <- eval env ps
  bindings <- matchParams "$define!" tree ps'
  traverse_ (uncurry (defineVar env)) bindings
define _ args = numArgs "$define!" 2 args

primEval :: Builtin r
primEval _ [e, x] = getEnvironment "eval" x >>= \env -> eval env e
primEval _ args = numArgs "eval" 2 args

primWrap :: Builtin r
primWrap _ [x] = LCombiner . ApplicativeCombiner <$> getCombiner "wrap" x
primWrap _ args = numArgs "wrap" 1 args

primUnwrap :: Builtin r
primUnwrap _ [x] = getCombiner "unwrap" x >>= \case
  ApplicativeCombiner c -> pure $ LCombiner c
  OperativeCombiner _ -> evalError "unwrap: combiner not applicative"
primUnwrap _ args = numArgs "unwrap" 1 args

primIf :: Builtin r
primIf env [cond, x, y] = do
  b <- getBool "$if" =<< eval env cond
  if b then eval env x else eval env y
primIf _ args = numArgs "$if" 3 args

makeEnvironment :: Builtin r
makeEnvironment _ args = do
  parents <- traverse (getEnvironment "make-environment") args
  env <- liftIO $ newEnvironment parents
  pure $ LEnv env

makeEncapsulation :: Builtin r
makeEncapsulation _ [] = do
  typeId <- liftIO newUnique
  let
    encapsulate = builtinApp $ \_ -> \case
      [x] -> pure $ LEncapsulation $ Encapsulation typeId x
      args -> numArgs "<encapsulate>" 1 args
    test = builtinApp $ \_ args -> do
      let
        check (LEncapsulation (Encapsulation i _)) = i == typeId
        check _ = False
      pure $ LBool $ all check args
    decapsulate = builtinApp $ \_ -> \case
      [x] -> do
        Encapsulation i val <- getEncapsulation "<decapsulate>" x
        when (i /= typeId) $ evalError "<decapsulate>: encapsulation mismatch"
        pure val
      args -> numArgs "<decapsulate>" 1 args
  pure $ listToExpr [encapsulate, test, decapsulate]
makeEncapsulation _ args = numArgs "make-encapsulation-type" 0 args

cons :: Builtin r
cons _ [x, y] = pure $ LPair x y
cons _ args = numArgs "cons" 2 args

binds :: Builtin r
binds env (envExpr:args) = do
  targetEnv <- eval env envExpr >>= \case
    LEnv targetEnv -> pure targetEnv
    x -> evalError $ "$binds?: expected environment, but got " <> renderType x
  syms <- traverse (getSymbol "$binds?") args
  let isBound sym = isJust <$> lookupVar targetEnv sym
  LBool <$> allM isBound syms
binds _ [] = numArgsAtLeast "$binds?" 1 []

