{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Builtins (mkBuiltins) where

import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map

import Types

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.ControlFlow (builtinControlFlow)
import Builtins.Definitions (builtinDefinitions)
import Builtins.Predicates (builtinPredicates)
import Builtins.Primitives (builtinPrimitives)

mkBuiltins :: IO Environment
mkBuiltins = do
    mappings <- Map.fromList <$> traverse ctxCell (concat builtins)
    Environment <$> newIORef mappings
  where
    ctxCell (name, bi) = (name,) <$> newIORef bi
    builtins =
      [ builtinBootstrap
      , builtinDefinitions
      , builtinControlFlow
      , builtinPredicates
      , builtinPrimitives
      ]

