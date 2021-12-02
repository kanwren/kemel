module Builtins.Utils where

import Types

builtinApp :: Builtin -> Expr
builtinApp f = LCombiner $ ApplicativeCombiner $ OperativeCombiner $ BuiltinOp f

builtinOp :: Builtin -> Expr
builtinOp f = LCombiner $ OperativeCombiner $ BuiltinOp f
