{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import TextShow (showt)

import Core (eval, defineVar, mkVau, combine, operate, wrap, unwrap, matchParams, parseParamTree)
import Errors
import Types
import Builtins.Utils (builtinOp, builtinApp, Builtin)

vau :: Builtin
vau (params:env:body) = do
  envName <- case env of
    LIgnore   -> pure IgnoreBinder
    LSymbol s -> pure $ NamedBinder s
    x -> evalError $ "$vau: invalid environment name: " <> showt x
  LCombiner <$> mkVau envName params body
vau args = numArgsAtLeast "$vau" 2 args

define :: Builtin
define [bs, ps] = LInert <$ do
  tree <- parseParamTree "$define!" bs
  env <- ask
  ps' <- eval env ps
  bindings <- matchParams "$define!" tree ps'
  traverse_ (\(name, val) -> defineVar name val env) bindings
define args = numArgsBound "$define!" (1, 2) args

primEval :: Builtin
primEval [e, LEnv env] = eval env e
primEval [_, x] = evalError $ "eval: expected environment, but got " <> renderType x
primEval args = numArgs "eval" 2 args

primWrap :: Builtin
primWrap [LCombiner c] = LCombiner <$> wrap c
primWrap [x] = evalError $ "wrap: expected combiner, but got " <> renderType x
primWrap args = numArgs "wrap" 1 args

primUnwrap :: Builtin
primUnwrap [LCombiner c] = LCombiner <$> unwrap c
primUnwrap [x] = evalError $ "unwrap: expected combiner, but got " <> renderType x
primUnwrap args = numArgs "unwrap" 1 args

primCombine :: Builtin
primCombine [LCombiner combiner, LList operands, LEnv env] = combine env combiner operands
primCombine [x, LList _, _     ] = evalError $ "combine: expected combiner, but got " <> renderType x
primCombine [_, x,       LEnv _] = evalError $ "combine: expected list of operands, but got " <> renderType x
primCombine [_, _,       x     ] = evalError $ "combine: expected environment, but got " <> renderType x
primCombine args = numArgs "combine" 3 args

primOperate :: Builtin
primOperate [LCombiner (OperativeCombiner combiner), LList operands, LEnv env] = operate env combiner operands
primOperate [LCombiner x, _, _]  = evalError $ "operate: not an operative: " <> showt x
primOperate [x, LList _, _     ] = evalError $ "operate: expected combiner, but got " <> renderType x
primOperate [_, x,       LEnv _] = evalError $ "operate: expected list of operands, but got " <> renderType x
primOperate [_, _,       x     ] = evalError $ "operate: expected environment, but got " <> renderType x
primOperate args = numArgs "operate" 3 args

builtinBootstrap :: [(Symbol, Expr)]
builtinBootstrap =
  [ ("$vau", builtinOp vau)
  , ("$define!", builtinOp define)
  , ("eval", builtinApp primEval)
  , ("wrap", builtinApp primWrap)
  , ("unwrap", builtinApp primUnwrap)
  , ("combine", builtinApp primCombine)
  , ("operate", builtinApp primOperate)
  ]

