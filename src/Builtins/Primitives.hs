{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Primitives (builtinPrimitives) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.Functor (($>), (<&>))
import Data.List (foldl', foldl1')
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit qualified as Exit
import TextShow (TextShow(..))

import Builtins.Utils (builtinApp)
import Core (evalFile)
import Errors
import Types

builtinPrimitives :: [(Symbol, Expr)]
builtinPrimitives = fmap (second builtinApp)
  [ ("not?", primNot)
  , ("and?", primAnd)
  , ("or?", primOr)
  , ("+", iadd)
  , ("-", isub)
  , ("*", imul)
  , ("div", iidiv)
  , ("mod", imod)
  , ("quot", iquot)
  , ("rem", irem)
  , ("=", ieq)
  , ("/=", ine)
  , (">", igt)
  , ("<", ilt)
  , (">=", ige)
  , ("<=", ile)
  , ("equal?", equal)
  , ("length", primLength)
  , ("string=", stringEq)
  , ("string>", stringGt)
  , ("string<", stringLt)
  , ("string>=", stringGe)
  , ("string<=", stringLe)
  , ("gensym", primGensym)
  , ("print", printExpr)
  , ("load", load)
  ]

primNot :: Builtin
primNot _ [LBool b] = pure $ LBool $ not b
primNot _ [x] = typeError "not?" "bool" x
primNot _ args = numArgs "not?" 1 args

primAnd :: Builtin
primAnd _ args = LBool . and <$> traverse (getBool "and?") args

primOr :: Builtin
primOr _ args = LBool . or <$> traverse (getBool "or?") args

asInts :: Symbol -> [Expr] -> Eval [Integer]
asInts name = go []
  where
    go :: [Integer] -> [Expr] -> Eval [Integer]
    go acc [] = pure $ reverse acc
    go acc (LInt x:xs) = go (x:acc) xs
    go _ (e:_) = typeError name "integer" e

iadd :: Builtin
iadd _ args = asInts "+" args <&> LInt . foldl' (+) 0

-- unary should be negation
isub :: Builtin
isub _ args = asInts "-" args <&> \case
  [x] -> LInt (-x)
  xs -> LInt $ foldl1' (-) xs

imul :: Builtin
imul _ args = asInts "*" args <&> LInt . foldl' (*) 1

-- unary should be reciprocal
iidiv :: Builtin
iidiv _ args = asInts "div" args >>= \case
  [x] -> pure $ LInt (1 `div` x)
  [] -> numArgsAtLeast "div" 1 []
  xs -> pure $ LInt $ foldl1' div xs

imod, iquot, irem :: Builtin
imod _ args = asInts "mod" args >>= \case
  [] -> numArgsAtLeast "mod" 1 []
  xs -> pure $ LInt $ foldl1' mod xs
iquot _ args = asInts "quot" args >>= \case
  [] -> numArgsAtLeast "quot" 1 []
  xs -> pure $ LInt $ foldl1' quot xs
irem _ args = asInts "rem" args >>= \case
  [] -> numArgsAtLeast "rem" 1 []
  xs -> pure $ LInt $ foldl1' rem xs

comparison :: Symbol -> (Integer -> Integer -> Bool) -> [Expr] -> Eval Expr
comparison name is args = asInts name args >>= \case
  [] -> numArgsAtLeast name 1 []
  xs -> pure $ LBool $ and $ zipWith is xs (tail xs)

ieq, ine, igt, ilt, ige, ile :: Builtin
ieq _ = comparison "=" (==)
ine _ = comparison "/=" (/=)
igt _ = comparison ">" (>)
ilt _ = comparison "<" (<)
ige _ = comparison ">=" (>=)
ile _ = comparison "<=" (<=)

primLength :: Builtin
primLength _ [LList xs] = pure $ LInt $ fromIntegral $ length xs
primLength _ [LString xs] = pure $ LInt $ fromIntegral $ Text.length xs
primLength _ [x] = typeError "length" "sequence" x
primLength _ args = numArgs "length" 1 args

stringComparison :: Symbol -> (forall e. Ord e => e -> e -> Bool) -> [Expr] -> Eval Expr
stringComparison _ cmp [LString x, LString y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LKeyword x, LKeyword y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LSymbol x, LSymbol y] = pure $ LBool (cmp x y)
stringComparison name _ [x, y] = evalError $ showt name <> ": invalid argument types " <> renderType x <> " and " <> renderType y
stringComparison name _ args = numArgs name 2 args

stringEq, stringGt, stringLt, stringGe, stringLe :: Builtin
stringEq _ = stringComparison "string=" (==)
stringGt _ = stringComparison "string>" (>)
stringLt _ = stringComparison "string<" (<)
stringGe _ = stringComparison "string>=" (<=)
stringLe _ = stringComparison "string<=" (<=)

equal :: Builtin
equal _ args =
  case args of
    [x, y] -> LBool <$> equal' x y
    _ -> numArgs "equal?" 2 args
  where
    equal' :: Expr -> Expr -> Eval Bool
    equal' LInert LInert = pure True
    equal' LIgnore LIgnore = pure True
    equal' (LInt x) (LInt y) = pure (x == y)
    equal' (LBool x) (LBool y) = pure (x == y)
    equal' (LKeyword x) (LKeyword y) = pure (x == y)
    equal' (LString x) (LString y) = pure (x == y)
    equal' (LSymbol x) (LSymbol y) = pure (x == y)
    equal' (LEnv x) (LEnv y) = pure (x == y)
    equal' (LList x) (LList y) =
      if length x /= length y
      then pure False
      else and <$> zipWithM equal' x y
    equal' (LDottedList x x') (LDottedList y y') =
      if length x /= length y
      then pure False
      else (&&) <$> (and <$> zipWithM equal' (NonEmpty.toList x) (NonEmpty.toList y)) <*> equal' x' y'
    equal' (LCombiner (ApplicativeCombiner c1)) (LCombiner (ApplicativeCombiner c2)) = equal' (LCombiner c1) (LCombiner c2)
    equal' (LCombiner (OperativeCombiner _)) (LCombiner (OperativeCombiner _)) = pure False -- TODO
    equal' x y = evalError $ "equal?: incompatible types " <> renderType x <> " and " <> renderType y

primGensym :: Builtin
primGensym _ [] = LSymbol <$> genSym
primGensym _ args = numArgs "gensym" 0 args

printExpr :: Builtin
printExpr _ [e] = liftIO (print e) $> LInert
printExpr _ args = numArgs "print" 1 args

load :: Builtin
load env [LString path] = do
  contents <- liftIO (Text.IO.readFile (Text.unpack path))
  evalFile env contents
load _ [e] = typeError "load" "string as path" e
load _ args = numArgs "load" 1 args
