{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Builtins.Bootstrap (builtinBootstrap) where

import Types
import Builtins.Utils (builtinOp, builtinApp)

builtinBootstrap :: [(Symbol, Expr)]
builtinBootstrap =
  [ ("$vau", builtinOp undefined)
  , ("$define!", builtinOp undefined)
  , ("eval", builtinApp primEval)
  , ("wrap", builtinApp primWrap)
  , ("unwrap", builtinApp primUnwrap)
  ]
  where
    primUnwrap = undefined
    primEval = undefined
    primWrap = undefined

