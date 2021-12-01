{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Builtins.ControlFlow (builtinControlFlow) where

import Control.Monad.Reader (ask)

import Errors
import Core (eval)
import Types
import Builtins.Utils (builtinOp, Builtin)

builtinControlFlow :: [(Symbol, Expr)]
builtinControlFlow =
  [ ("$if", builtinOp primIf)
  , ("$block", builtinOp primBlock)
  , ("$return-from", builtinOp primReturnFrom)
  , ("$tagbody", builtinOp primTagbody)
  , ("$go", builtinOp primGo)
  ]

truthy :: Expr -> Bool
truthy = \case
  LBool b -> b
  _ -> True

condition :: Environment -> Expr -> Eval Bool
condition env cond = truthy <$> eval env cond

primIf :: Builtin
primIf args = do
  env <- ask
  case args of
    [cond, x, y] -> do
      cond' <- condition env cond
      if cond' then eval env x else eval env y
    _ -> numArgs "if" 3 args

primBlock :: Builtin
primBlock = undefined -- TODO

primReturnFrom :: Builtin
primReturnFrom = undefined -- TODO

primTagbody :: Builtin
primTagbody = undefined -- TODO

primGo :: Builtin
primGo = undefined -- TODO
