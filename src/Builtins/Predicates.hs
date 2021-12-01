{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Predicates (builtinPredicates) where

import TextShow (showt)
import Control.Monad.Reader (ask)

import Builtins.Utils (builtinApp, Builtin, builtinOp)
import Core (eval)
import Errors
import Types

allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = foldr (andM . p) (pure True)
  where andM a b = a >>= \a' -> if a' then b else pure False

anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr (orM . p) (pure False)
  where orM a b = a >>= \a' -> if a' then pure True else b

typep :: Symbol -> Expr -> Expr -> Eval Bool
typep name v = go
  where
    go (LSymbol s) =
      case symbolToTypePred s of
        Just p  -> pure $ p v
        Nothing -> evalError $ showt name <> ": invalid type specifier"
    go (LList (LSymbol "and":spec)) = allM go spec
    go (LList (LSymbol "or":spec)) = anyM go spec
    go (LList (LSymbol "not":spec)) =
      case spec of
        [p] -> not <$> go p
        _ -> evalError $ showt name <> ": expected exactly 1 argument to not, but got " <> showt (length spec)
    go (LList (LSymbol "integer":spec)) =
      case v of
        LInt n ->
          case spec of
            [LInt lower] -> pure $ lower <= n
            [LList [LInt lower]] -> pure $ lower <= n
            [LInt lower, LInt upper] -> pure $ lower <= n && n <= upper
            _ -> evalError $ showt name <> ": invalid type specifier: invalid arguments to predicate integer"
        _ -> pure False
    go _ = evalError $ showt name <> ": invalid type specifier"

primThe :: Builtin
primThe [spec, v] = do
  env <- ask
  v' <- eval env v
  valid <- typep "the" v' spec
  if valid
    then pure v'
    else evalError $ "the: expected type " <> showt spec <> ", but value " <> showt v' <> " has type " <> renderType v'
primThe args = numArgs "the" 2 args

primTypep :: Builtin
primTypep [v, e] = LBool <$> typep "typep" v e
primTypep args = numArgs "typep" 2 args

typePred :: Symbol -> Symbol -> Builtin
typePred name s = \case
  [v] -> LBool <$> typep name v (LSymbol s)
  args -> numArgs name 1 args

builtinPredicates :: [(Symbol, Expr)]
builtinPredicates =
  [ ("the", builtinOp primThe)
  , ("typep", builtinApp primTypep)
  , ("numberp", builtinApp (typePred "numberp" "number"))
  , ("integerp", builtinApp (typePred "integerp" "integer"))
  , ("ratiop", builtinApp (typePred "ratiop" "ratio"))
  , ("rationalp", builtinApp (typePred "rationalp" "rational"))
  , ("boolp", builtinApp (typePred "boolp" "bool"))
  , ("charp", builtinApp (typePred "charp" "char"))
  , ("keywordp", builtinApp (typePred "keywordp" "keyword"))
  , ("stringp", builtinApp (typePred "stringp" "string"))
  , ("symbolp", builtinApp (typePred "symbolp" "symbol"))
  , ("null", builtinApp (typePred "null" "null"))
  , ("listp", builtinApp (typePred "listp" "list"))
  , ("consp", builtinApp (typePred "consp" "cons"))
  , ("functionp", builtinApp (typePred "functionp" "function"))
  , ("macrop", builtinApp (typePred "macrop" "macro"))
  ]
