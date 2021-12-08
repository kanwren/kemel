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

typeError :: MonadError Error m => Symbol -> Text -> Expr r -> m a
typeError name expected got = evalError $ showt name <> ": expected " <> expected <> ", but got " <> renderType got

numArgs :: Symbol -> Int -> [Expr r] -> Eval r a
numArgs name n xs
  | got > n = evalError $ showt name <> ": too many args: expected " <> showt n <> " but got " <> showt got
  | got < n = evalError $ showt name <> ": too few args: expected " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgs: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

numArgsAtLeast :: Symbol -> Int -> [Expr r] -> Eval r a
numArgsAtLeast name n xs
  | got < n = evalError $ showt name <> ": too few args: expected at least " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgsAtLeast: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

numArgsBound :: Symbol -> (Int, Int) -> [Expr r] -> Eval r a
numArgsBound name (minlen, maxlen) xs
  | got < minlen = evalError $ showt name <> ": too few args: expected at least " <> showt minlen <> " but got " <> showt got
  | got > maxlen = evalError $ showt name <> ": too many args: expected at most " <> showt maxlen <> " but got " <> showt got
  | otherwise = error $ "numArgsBound: impossible: " ++ Text.unpack (showt name)
  where
    got = length xs

getInert :: Symbol -> Expr r -> Eval r ()
getInert name = \case
  LInert -> pure ()
  x -> typeError name "inert" x

getIgnore :: Symbol -> Expr r -> Eval r ()
getIgnore name = \case
  LIgnore -> pure ()
  x -> typeError name "ignore" x

getInteger :: Symbol -> Expr r -> Eval r Integer
getInteger name = \case
  LInt n -> pure n
  x -> typeError name "integer" x

getBool :: Symbol -> Expr r -> Eval r Bool
getBool name = \case
  LBool b -> pure b
  x -> typeError name "bool" x

getKeyword :: Symbol -> Expr r -> Eval r Keyword
getKeyword name = \case
  LKeyword k -> pure k
  x -> typeError name "keyword" x

getString :: Symbol -> Expr r -> Eval r Text
getString name = \case
  LString s -> pure s
  x -> typeError name "string" x

getSymbol :: Symbol -> Expr r -> Eval r Symbol
getSymbol name = \case
  LSymbol s -> pure s
  x -> typeError name "symbol" x

getEncapsulation :: Symbol -> Expr r -> Eval r (Encapsulation r)
getEncapsulation name = \case
  LEncapsulation e -> pure e
  x -> typeError name "encapsulation" x

getEnvironment :: Symbol -> Expr r -> Eval r (Environment r)
getEnvironment name = \case
  LEnv e -> pure e
  x -> typeError name "environment" x

getContinuation :: Symbol -> Expr r -> Eval r (Expr r -> Eval r (Expr r))
getContinuation name = \case
  LContinuation c -> pure c
  x -> typeError name "continuation" x

getList :: Symbol -> Expr r -> Eval r (NonEmpty (Expr r))
getList name = \case
  LPair x xs ->
    case pairToList x xs of
      Right ys -> pure ys
      Left _ -> evalError $ showt name <> ": expected proper list, but got improper list"
  x -> typeError name "list" x

getCombiner :: Symbol -> Expr r -> Eval r (Combiner r)
getCombiner name = \case
  LCombiner x -> pure x
  x -> typeError name "combiner" x

getApplicative :: Symbol -> Expr r -> Eval r (Combiner r)
getApplicative name = \case
  LCombiner (ApplicativeCombiner x) -> pure x
  x -> typeError name "applicative" x

