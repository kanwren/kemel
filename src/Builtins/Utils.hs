{-# LANGUAGE ImportQualifiedPost #-}

module Builtins.Utils where

import Types

type Builtin = [Expr] -> Eval Expr

builtinApp :: ([Expr] -> Eval Expr) -> Expr
builtinApp f = LCombiner $ ApplicativeCombiner $ OperativeCombiner $ BuiltinFun f

builtinOp :: ([Expr] -> Eval Expr) -> Expr
builtinOp f = LCombiner $ OperativeCombiner $ BuiltinFun f
