{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Builtins (makeGround) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.IO qualified as Text.IO

import Builtins.Bootstrap (builtinBootstrap)
import Builtins.Continuations (builtinContinuations)
import Builtins.TypeOps (builtinTypeOps)
import Builtins.Primitives (builtinPrimitives)
import Core (evalFile, defineVar)
import Types

import Paths_kemel

makeGround :: (Expr r -> Eval r (Expr r)) -> Eval r (Environment r)
makeGround rootContinuation = do
  let
    builtins =
      [ builtinBootstrap
      , builtinContinuations
      , builtinTypeOps
      , builtinPrimitives
      ]
  env <- liftIO $ newEnvironmentWith (concat builtins) []
  defineVar env "root-continuation" (LContinuation rootContinuation)
  contents <- liftIO $ do
    preludeFile <- getDataFileName "stdlib/prelude.kml"
    Text.IO.readFile preludeFile
  _ <- evalFile env contents
  liftIO $ newEnvironment [env]

