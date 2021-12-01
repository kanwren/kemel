{-# LANGUAGE ImportQualifiedPost #-}

module Builtins.Utils where

import Types

type Builtin = [Expr] -> Eval Expr

builtinApp :: ([Expr] -> Eval Expr) -> Expr
builtinApp f = LCombiner $ ApplicativeCombiner $ OperativeCombiner $ BuiltinOp f

builtinOp :: ([Expr] -> Eval Expr) -> Expr
builtinOp f = LCombiner $ OperativeCombiner $ BuiltinOp f
