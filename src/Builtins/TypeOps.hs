{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.TypeOps (builtinTypeOps) where

import Data.CaseInsensitive (mk)
import TextShow (showt)

import Builtins.Utils (builtinApp, builtinOp, allM, anyM)
import Core (eval)
import Errors
import Types

typep :: Symbol -> Expr -> Expr -> Eval Bool
typep name v = go
  where
    go :: Expr -> Eval Bool
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

builtinTypeOps :: [(Symbol, Expr)]
builtinTypeOps = helpers <> typePreds
  where
    helpers =
      [ ("type-of", builtinApp typeOf)
      , ("the", builtinOp primThe)
      , ("type?", builtinApp primTypep)
      ]
    typePreds = fmap mkPred
      [ "inert"
      , "ignore"
      , "number"
      , "integer"
      , "bool"
      , "keyword"
      , "string"
      , "symbol"
      , "environment"
      , "null"
      , "list"
      , "pair"
      , "combiner"
      , "operative"
      , "applicative"
      ]
    mkPred name =
      let
        typeName = SimpleSymbol $ mk name
        predName = SimpleSymbol $ mk $ name <> "?"
        app _ = fmap (LBool . and) . traverse (\v -> typep predName v (LSymbol typeName))
      in (predName, builtinApp app)

typeOf :: Builtin
typeOf _ [v] = pure $ LSymbol $ typeToSymbol v
typeOf _ args = numArgs "type-of" 1 args

primThe :: Builtin
primThe env [spec, v] = do
  v' <- eval env v
  valid <- typep "the" v' spec
  if valid
    then pure v'
    else evalError $ "the: expected type " <> showt spec <> ", but value " <> showt v' <> " has type " <> renderType v'
primThe _ args = numArgs "the" 2 args

primTypep :: Builtin
primTypep _ [v, e] = LBool <$> typep "type?" v e
primTypep _ args = numArgs "type?" 2 args
