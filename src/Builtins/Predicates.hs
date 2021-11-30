{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Predicates (builtinPredicates) where

import Data.Bifunctor (second)

import Errors
import Core (typep)
import Types
import Builtins.Utils (builtinApp, Builtin)

builtinPredicates :: [(Symbol, Expr)]
builtinPredicates = fmap (second builtinApp)
  [ ("typep", primTypep)
  , ("numberp", typePred "numberp" "number")
  , ("integerp", typePred "integerp" "integer")
  , ("ratiop", typePred "ratiop" "ratio")
  , ("rationalp", typePred "rationalp" "rational")
  , ("boolp", typePred "boolp" "bool")
  , ("charp", typePred "charp" "char")
  , ("keywordp", typePred "keywordp" "keyword")
  , ("stringp", typePred "stringp" "string")
  , ("symbolp", typePred "symbolp" "symbol")
  , ("null", typePred "null" "null")
  , ("listp", typePred "listp" "list")
  , ("consp", typePred "consp" "cons")
  , ("functionp", typePred "functionp" "function")
  , ("macrop", typePred "macrop" "macro")
  ]
  where
    primTypep :: Builtin
    primTypep [v, e] = LBool <$> typep "typep" v e
    primTypep args = numArgs "typep" 2 args

    typePred :: Symbol -> Symbol -> Builtin
    typePred name s = \case
      [v] -> LBool <$> typep name v (LSymbol s)
      args -> numArgs name 1 args

