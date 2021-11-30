{-# LANGUAGE OverloadedStrings #-}

module Builtins.Definitions (builtinDefinitions) where

import Core (nil)
import Types

builtinDefinitions :: [(Symbol, Expr)]
builtinDefinitions =
  [ ("nil", nil)
  ]

