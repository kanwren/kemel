{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Errors where

import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))

import Types

evalError :: MonadError Error m => Text -> m a
evalError = throwError . EvalError

typeError :: MonadError Error m => Symbol -> Text -> Expr -> m a
typeError name expected got = evalError $ showt name <> ": expected " <> expected <> ", but got " <> renderType got

numArgs :: Symbol -> Int -> [Expr] -> Eval a
numArgs name n xs
  | got > n = evalError $ showt name <> ": too many args: expected " <> showt n <> " but got " <> showt got
  | got < n = evalError $ showt name <> ": too few args: expected " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgs: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

numArgsAtLeast :: Symbol -> Int -> [Expr] -> Eval a
numArgsAtLeast name n xs
  | got < n = evalError $ showt name <> ": too few args: expected at least " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgsAtLeast: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

numArgsBound :: Symbol -> (Int, Int) -> [Expr] -> Eval a
numArgsBound name (minlen, maxlen) xs
  | got < minlen = evalError $ showt name <> ": too few args: expected at least " <> showt minlen <> " but got " <> showt got
  | got > maxlen = evalError $ showt name <> ": too many args: expected at most " <> showt maxlen <> " but got " <> showt got
  | otherwise = error $ "numArgsBound: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

getInert :: Symbol -> Expr -> Eval ()
getInert name = \case
  LInert -> pure ()
  x -> typeError name "inert" x

getIgnore :: Symbol -> Expr -> Eval ()
getIgnore name = \case
  LIgnore -> pure ()
  x -> typeError name "ignore" x

getInteger :: Symbol -> Expr -> Eval Integer
getInteger name = \case
  LInt n -> pure n
  x -> typeError name "integer" x

getBool :: Symbol -> Expr -> Eval Bool
getBool name = \case
  LBool b -> pure b
  x -> typeError name "bool" x

getKeyword :: Symbol -> Expr -> Eval Keyword
getKeyword name = \case
  LKeyword k -> pure k
  x -> typeError name "keyword" x

getString :: Symbol -> Expr -> Eval Text
getString name = \case
  LString s -> pure s
  x -> typeError name "string" x

getSymbol :: Symbol -> Expr -> Eval Symbol
getSymbol name = \case
  LSymbol s -> pure s
  x -> typeError name "symbol" x

getEnvironment :: Symbol -> Expr -> Eval Environment
getEnvironment name = \case
  LEnv e -> pure e
  x -> typeError name "environment" x

getList :: Symbol -> Expr -> Eval [Expr]
getList name = \case
  LList xs -> pure xs
  x -> typeError name "list" x

getDottedList :: Symbol -> Expr -> Eval (NonEmpty Expr, Expr)
getDottedList name = \case
  LDottedList xs x -> pure (xs, x)
  x -> typeError name "list" x

getCombiner :: Symbol -> Expr -> Eval Combiner
getCombiner name = \case
  LCombiner x -> pure x
  x -> typeError name "combiner" x

