{-# LANGUAGE ImportQualifiedPost #-}

module Builtins (loadPrelude) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.IO qualified as Text.IO

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.ControlFlow (builtinControlFlow)
import Builtins.TypeOps (builtinTypeOps)
import Builtins.Primitives (builtinPrimitives)
import Core (evalFile)
import Types

import Paths_kemel

loadPrelude :: Eval Environment
loadPrelude = do
    env <- liftIO $ newEnvironmentWith (concat builtins) []
    contents <- liftIO $ do
      preludeFile <- getDataFileName "prelude/lib.lsp"
      Text.IO.readFile preludeFile
    _ <- evalFile env contents
    pure env
  where
    builtins =
      [ builtinBootstrap
      , builtinControlFlow
      , builtinTypeOps
      , builtinPrimitives
      ]

