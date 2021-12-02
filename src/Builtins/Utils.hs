module Builtins.Utils where

import Types

builtinApp :: Builtin -> Expr
builtinApp f = LCombiner $ ApplicativeCombiner $ OperativeCombiner $ BuiltinOp f

builtinOp :: Builtin -> Expr
builtinOp f = LCombiner $ OperativeCombiner $ BuiltinOp f

allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = foldr (andM . p) (pure True)
  where andM a b = a >>= \a' -> if a' then b else pure False

anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr (orM . p) (pure False)
  where orM a b = a >>= \a' -> if a' then pure True else b
