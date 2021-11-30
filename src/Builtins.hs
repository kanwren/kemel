{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Builtins (mkBuiltins) where

import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map

import Types

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.Definitions (builtinDefinitions)
import Builtins.Primitives (builtinPrimitives)
import Builtins.Predicates (builtinPredicates)

mkBuiltins :: IO Environment
mkBuiltins = do
    mappings <- Map.fromList <$> traverse ctxCell (concat builtins)
    Environment <$> newIORef mappings
  where
    ctxCell (name, bi) = (name,) <$> newIORef bi
    builtins =
      [ builtinBootstrap
      , builtinDefinitions
      , builtinPredicates
      , builtinPrimitives
      ]

