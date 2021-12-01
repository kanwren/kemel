{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Builtins.ControlFlow (builtinControlFlow) where

import Control.Monad.Reader (ask)

import Errors
import Core (eval, progn)
import Types
import Builtins.Utils (builtinOp, Builtin)

builtinControlFlow :: [(Symbol, Expr)]
builtinControlFlow =
  [ ("$if", builtinOp primIf)
  , ("$sequence", builtinOp primSequence)
  , ("$block", builtinOp primBlock)
  , ("$return-from", builtinOp primReturnFrom)
  , ("$tagbody", builtinOp primTagbody)
  , ("$go", builtinOp primGo)
  ]

primIf :: Builtin
primIf [cond, x, y] = do
  env <- ask
  eval env cond >>= \case
    LBool b -> if b then eval env x else eval env y
    e       -> evalError $ "$if: expected boolean condition but got " <> renderType e
primIf args = numArgs "$if" 3 args

primSequence :: Builtin
primSequence args = do
  env <- ask
  progn env args

primBlock :: Builtin
primBlock = undefined -- TODO

primReturnFrom :: Builtin
primReturnFrom = undefined -- TODO

primTagbody :: Builtin
primTagbody = undefined -- TODO

primGo :: Builtin
primGo = undefined -- TODO
