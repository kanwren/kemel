{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Core where

import Control.Monad.Except
import Control.Monad.Writer
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))
import qualified Data.HashTable.IO as HIO

import Errors
import Parser (parseFile)
import Types

defineVar :: Environment r -> Symbol -> Expr r -> Eval r ()
defineVar (Environment table _) i v = liftIO $ HIO.insert table i v

lookupVar :: Environment r -> Symbol -> Eval r (Maybe (Expr r))
lookupVar (Environment table parents) i = do
  mapping <- liftIO $ HIO.lookup table i
  case mapping of
    Just x  -> pure $ Just x
    Nothing -> do
      let
        go [] = pure Nothing
        go (env:envs) = lookupVar env i >>= \case
          Just x -> pure $ Just x
          Nothing -> go envs
      go parents

parseParamTree :: Symbol -> Expr r -> Eval r ParamTree
parseParamTree name e = go e
  where
    go LIgnore = pure IgnoreParam
    go (LSymbol s) = pure $ BoundParam s
    go LNull = pure ParamNull
    go (LPair p ps) = ParamPair <$> go p <*> go ps
    go _ = evalError $ showt name <> ": invalid parameter tree: " <> showt e

matchParams :: Symbol -> ParamTree -> Expr r -> Eval r [(Symbol, Expr r)]
matchParams name tree args = liftEither $ runExcept $ execWriterT $ go tree args
  where
    toError :: Text -> WriterT [(Symbol, Expr r)] (Except Error) ()
    toError e = evalError $ showt name <> ": " <> e <> "; " <> showt (renderTree tree) <> " <- " <> showt args
      where
        renderTree :: ParamTree -> Expr r
        renderTree IgnoreParam = LIgnore
        renderTree (BoundParam s) = LSymbol s
        renderTree ParamNull = LNull
        renderTree (ParamPair p ps) = LPair (renderTree p) (renderTree ps)

    go :: ParamTree -> Expr r -> WriterT [(Symbol, Expr r)] (Except Error) ()
    go IgnoreParam _ = pure ()
    go (BoundParam b) p = tell [(b, p)]
    go ParamNull LNull = pure ()
    go ParamNull _ = toError "too many values when unpacking list"
    go (ParamPair x xs) (LPair y ys) = go x y *> go xs ys
    go (ParamPair _ _) _ = toError "not enough values when unpacking list"

-- Evaluate a list of expressions and return the value of the final expression
progn :: Environment r -> [Expr r] -> Eval r (Expr r)
progn env = go
  where
    go [] = pure LInert
    go [x] = eval env x
    go (x:y) = eval env x *> go y

eval :: Environment r -> Expr r -> Eval r (Expr r)
eval env = \case
  LSymbol sym -> lookupVar env sym >>= \case
    Just x  -> pure x
    Nothing -> evalError $ "variable not in scope: " <> showt sym
  LNull            -> pure LNull
  LPair f args -> eval env f >>= \case
    LCombiner c ->
      let discardDot x xs = either (NonEmpty.tail . fst) NonEmpty.tail $ pairToList x xs
      in combine env c $ discardDot f args
    e -> evalError $ "expected combiner in call, but " <> showt f <> " was " <> renderType e <> ": " <> showt e
  -- self-evaluating expressions
  f -> pure f

combine :: Environment r -> Combiner r -> [Expr r] -> Eval r (Expr r)
combine env (OperativeCombiner c) args = operate env c args
combine env (ApplicativeCombiner c) args = traverse (eval env) args >>= combine env c

operate :: Environment r -> Operative r -> [Expr r] -> Eval r (Expr r)
operate env c args = do
  case c of
    BuiltinOp f -> f env args
    UserOp Closure{..} -> do
      paramBinds <- matchParams "<combiner>" closureParams (listToExpr args)
      let binds = maybe paramBinds (\n -> (n, LEnv env):paramBinds) closureDynamicEnv
      env' <- liftIO $ newEnvironmentWith binds [closureStaticEnv]
      eval env' closureBody

evalFile :: Environment r -> Text -> Eval r (Expr r)
evalFile env contents =
  case parseFile contents of
    Right res -> progn env res
    -- TODO: fix this error message
    Left e -> evalError $ "load: parse error: " <> Text.pack e
