{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust)
import Data.Unique (newUnique)
import TextShow (showt)

import Builtins.Utils (builtinOp, builtinApp, allM)
import Core (eval, parseParamTree, matchParams, lookupVar, progn)
import Errors
import Types
import qualified Data.HashTable.IO as HIO

builtinBootstrap :: [(Symbol, Expr)]
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
vau :: Builtin
vau staticEnv [params, env, body] = do
  envName <- case env of
    LIgnore   -> pure IgnoreBinder
    LSymbol s -> pure $ NamedBinder s
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

define :: Builtin
define env@(Environment table _) [bs, ps] = LInert <$ do
  tree <- parseParamTree "$define!" bs
  ps' <- eval env ps
  bindings <- matchParams "$define!" tree ps'
  liftIO $ traverse_ (uncurry (HIO.insert table)) bindings
define _ args = numArgs "$define!" 2 args

primEval :: Builtin
primEval _ [e, x] = getEnvironment "eval" x >>= \env -> eval env e
primEval _ args = numArgs "eval" 2 args

primWrap :: Builtin
primWrap _ [x] = LCombiner . ApplicativeCombiner <$> getCombiner "wrap" x
primWrap _ args = numArgs "wrap" 1 args

primUnwrap :: Builtin
primUnwrap _ [x] = getCombiner "unwrap" x >>= \case
  ApplicativeCombiner c -> pure $ LCombiner c
  OperativeCombiner _ -> evalError "unwrap: combiner not applicative"
primUnwrap _ args = numArgs "unwrap" 1 args

primIf :: Builtin
primIf env [cond, x, y] = do
  b <- getBool "$if" =<< eval env cond
  if b then eval env x else eval env y
primIf _ args = numArgs "$if" 3 args

makeEnvironment :: Builtin
makeEnvironment _ args = do
  parents <- traverse (getEnvironment "make-environment") args
  env <- liftIO $ newEnvironment parents
  pure $ LEnv env

makeEncapsulation :: Builtin
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
  pure $ LList [encapsulate, test, decapsulate]
makeEncapsulation _ args = numArgs "make-encapsulation-type" 0 args

cons :: Builtin
cons _ [x, LList y] = pure $ LList (x:y)
cons _ [x, LDottedList (y :| ys) z] = pure $ LDottedList (x :| (y : ys)) z
cons _ [x, y] = pure $ LDottedList (x :| []) y
cons _ args = numArgs "cons" 2 args

binds :: Builtin
binds env (envExpr:args) = do
  targetEnv <- eval env envExpr >>= \case
    LEnv targetEnv -> pure targetEnv
    x -> evalError $ "$binds?: expected environment, but got " <> renderType x
  syms <- traverse (getSymbol "$binds?") args
  let isBound sym = isJust <$> lookupVar sym targetEnv
  LBool <$> allM isBound syms
binds _ [] = numArgsAtLeast "$binds?" 1 []

