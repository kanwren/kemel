{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Builtins (loadPrelude) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import qualified Data.Text.IO as Text.IO

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.ControlFlow (builtinControlFlow)
import Builtins.Predicates (builtinPredicates)
import Builtins.Primitives (builtinPrimitives)
import Core (evalFile)
import Types

import Paths_kemel

loadPrelude :: Eval Environment
loadPrelude = do
    env <- liftIO $ do
      mappings <- Map.fromList <$> traverse ctxCell (concat builtins)
      Environment <$> newIORef mappings
    contents <- liftIO $ do
      preludeFile <- getDataFileName "prelude/lib.lsp"
      Text.IO.readFile preludeFile
    _ <- evalFile env contents
    pure env
  where
    ctxCell (name, bi) = (name,) <$> newIORef bi
    builtins =
      [ builtinBootstrap
      , builtinControlFlow
      , builtinPredicates
      , builtinPrimitives
      ]

