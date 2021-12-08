{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.TypeOps (builtinTypeOps) where

import Data.CaseInsensitive (mk)
import TextShow (showt)

import Builtins.Utils (builtinApp, builtinOp, allM, anyM)
import Core (eval)
import Errors
import Types

typep :: Symbol -> Expr r -> Expr r -> Eval r Bool
typep name v = go
  where
    go :: Expr r -> Eval r Bool
    go (LSymbol s) =
      case symbolToTypePred s of
        Just p  -> pure $ p v
        Nothing -> evalError $ showt name <> ": invalid type specifier"
    go (LPair (LSymbol "and") spec) = allM go =<< getList name spec
    go (LPair (LSymbol "or") spec) = anyM go =<< getList name spec
    go (LPair (LSymbol "not") spec) =
      case spec of
        LPair p LNull -> not <$> go p
        _ -> evalError $ showt name <> ": invalid arguments to not predicate: " <> showt spec
    go (LPair (LSymbol "integer") spec) =
      case v of
        LInt n ->
          case spec of
            LPair (LInt lower) LNull -> pure $ lower <= n
            LPair (LPair (LInt lower) LNull) LNull -> pure $ lower <= n
            LPair (LInt lower) (LPair (LInt upper) LNull) -> pure $ lower <= n && n <= upper
            _ -> evalError $ showt name <> ": invalid type specifier: invalid arguments to predicate integer"
        _ -> pure False
    go _ = evalError $ showt name <> ": invalid type specifier"

builtinTypeOps :: [(Symbol, Expr r)]
builtinTypeOps = helpers <> typePreds
  where
    helpers =
      [ ("type-of", builtinApp typeOf)
      , ("$the", builtinOp primThe)
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
      , "continuation"
      , "null"
      , "list"
      , "pair"
      , "combiner"
      , "operative"
      , "applicative"
      ]
    mkPred name =
      let
        typeName = Symbol (mk name)
        predName = Symbol (mk (name <> "?"))
        app _ = fmap (LBool . and) . traverse (\v -> typep predName v (LSymbol typeName))
      in (predName, builtinApp app)

typeOf :: Builtin r
typeOf _ [v] = pure $ LSymbol $ typeToSymbol v
typeOf _ args = numArgs "type-of" 1 args

primThe :: Builtin r
primThe env [spec, v] = do
  v' <- eval env v
  valid <- typep "$the" v' spec
  if valid
    then pure v'
    else evalError $ "$the: expected type " <> showt spec <> ", but value " <> showt v' <> " has type " <> renderType v'
primThe _ args = numArgs "$the" 2 args

primTypep :: Builtin r
primTypep _ [v, e] = LBool <$> typep "type?" v e
primTypep _ args = numArgs "type?" 2 args
