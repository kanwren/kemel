{-# LANGUAGE ImportQualifiedPost #-}

module Builtins.Utils where

import Types

type Builtin = [Expr] -> Eval Expr

builtinApp :: ([Expr] -> Eval Expr) -> Expr
builtinApp f = LCombiner $ Combiner { combinerType = ApplicativeCombiner, combinerFun = BuiltinFun f }

builtinOp :: ([Expr] -> Eval Expr) -> Expr
builtinOp f = LCombiner $ Combiner { combinerType = OperativeCombiner, combinerFun = BuiltinFun f }
