{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.ControlFlow (builtinControlFlow) where

import Builtins.Utils (builtinOp)
import Core (eval, progn)
import Errors
import Types

builtinControlFlow :: [(Symbol, Expr)]
builtinControlFlow =
  [ ("$if", builtinOp primIf)
  -- TODO: this can be defined by the standard library, but it is probably
  -- significantly faster to do this way rather than bootstrap
  , ("$sequence", builtinOp progn)
  , ("$block", builtinOp primBlock)
  , ("$return-from", builtinOp primReturnFrom)
  , ("$tagbody", builtinOp primTagbody)
  , ("$go", builtinOp primGo)
  ]

primIf :: Builtin
primIf env [cond, x, y] = do
  eval env cond >>= \case
    LBool b -> if b then eval env x else eval env y
    e       -> evalError $ "$if: expected boolean condition but got " <> renderType e
primIf _ args = numArgs "$if" 3 args

primBlock :: Builtin
primBlock = undefined -- TODO

primReturnFrom :: Builtin
primReturnFrom = undefined -- TODO

primTagbody :: Builtin
primTagbody = undefined -- TODO

primGo :: Builtin
primGo = undefined -- TODO
