{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Control.Monad.Reader (ask)
import TextShow (showt)

import Core (nil, eval, defineVar, mkVau, combine, operate, wrap, unwrap)
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
define [LIgnore, _] = pure nil
define [LSymbol name, x] = LInert <$ do
  env <- ask
  val <- eval env x
  defineVar name val env
define [LList xs, LList vs] = LInert <$ do
  let
    go [] [] = pure ()
    go (LSymbol x:xs') (v:vs') = do
      env <- ask
      val <- eval env v
      defineVar x val env
      go xs' vs'
    go (x:_) (_:_) = evalError $ "$define!: expected symbol, but got " <> renderType x
    go _ _ = evalError "$define!: mismatched binders and values"
  go xs vs
define [e, _] = evalError $ "$define!: expected symbol, but got " <> renderType e
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

