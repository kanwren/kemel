{-# LANGUAGE ImportQualifiedPost #-}

module Builtins (makeGround) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.IO qualified as Text.IO

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.TypeOps (builtinTypeOps)
import Builtins.Primitives (builtinPrimitives)
import Core (evalFile)
import Types

import Paths_kemel

makeGround :: Eval Environment
makeGround = do
  let
    builtins =
      [ builtinBootstrap
      , builtinTypeOps
      , builtinPrimitives
      ]
  env <- liftIO $ newEnvironmentWith (concat builtins) []
  contents <- liftIO $ do
    preludeFile <- getDataFileName "prelude.lsp"
    Text.IO.readFile preludeFile
  _ <- evalFile env contents
  liftIO $ newEnvironment [env]

