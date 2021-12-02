{-# LANGUAGE OverloadedStrings #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Data.Foldable (traverse_)
import TextShow (showt)

import Builtins.Utils (builtinOp, builtinApp)
import Core (eval, defineVar, mkVau, wrap, unwrap, matchParams, parseParamTree)
import Errors
import Types

builtinBootstrap :: [(Symbol, Expr)]
builtinBootstrap =
  [ ("$vau", builtinOp vau)
  , ("$define!", builtinOp define)
  , ("eval", builtinApp primEval)
  , ("wrap", builtinApp primWrap)
  , ("unwrap", builtinApp primUnwrap)
  ]

-- NOTE: this is later redefined in the prelude to be able to compensate for
-- automatically $sequencing bodies
vau :: Builtin
vau staticEnv [params, env, body] = do
  envName <- case env of
    LIgnore   -> pure IgnoreBinder
    LSymbol s -> pure $ NamedBinder s
    x -> evalError $ "$vau: invalid environment name: " <> showt x
  LCombiner <$> mkVau staticEnv envName params body
vau _ args = numArgs "$vau" 3 args

define :: Builtin
define env [bs, ps] = LInert <$ do
  tree <- parseParamTree "$define!" bs
  ps' <- eval env ps
  bindings <- matchParams "$define!" tree ps'
  traverse_ (\(name, val) -> defineVar name val env) bindings
define _ args = numArgsBound "$define!" (1, 2) args

primEval :: Builtin
primEval _ [e, LEnv env] = eval env e
primEval _ [_, x] = evalError $ "eval: expected environment, but got " <> renderType x
primEval _ args = numArgs "eval" 2 args

primWrap :: Builtin
primWrap _ [LCombiner c] = LCombiner <$> wrap c
primWrap _ [x] = evalError $ "wrap: expected combiner, but got " <> renderType x
primWrap _ args = numArgs "wrap" 1 args

primUnwrap :: Builtin
primUnwrap _ [LCombiner c] = LCombiner <$> unwrap c
primUnwrap _ [x] = evalError $ "unwrap: expected combiner, but got " <> renderType x
primUnwrap _ args = numArgs "unwrap" 1 args

