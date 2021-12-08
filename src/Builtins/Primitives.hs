{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Primitives (builtinPrimitives) where

import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.CaseInsensitive (mk, foldedCase)
import Data.Functor (($>), (<&>))
import Data.List (foldl', foldl1')
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import TextShow (TextShow(..))

import Builtins.Utils (builtinApp)
import Core (evalFile)
import Errors
import Types

import Paths_kemel

builtinPrimitives :: [(Symbol, Expr r)]
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
  , ("string-append", stringAppend)
  , ("string->symbol", stringToSymbol)
  , ("symbol->string", symbolToString)
  , ("gensym", primGensym)
  , ("print", primPrint)
  , ("write", primWrite)
  , ("display", primDisplay)
  , ("load", load)
  , ("get-data-file-path", getDataFilePath)
  ]

primNot :: Builtin r
primNot _ [LBool b] = pure $ LBool $ not b
primNot _ [x] = typeError "not?" "bool" x
primNot _ args = numArgs "not?" 1 args

primAnd :: Builtin r
primAnd _ args = LBool . and <$> traverse (getBool "and?") args

primOr :: Builtin r
primOr _ args = LBool . or <$> traverse (getBool "or?") args

asInts :: Symbol -> [Expr r] -> Eval r [Integer]
asInts name = go []
  where
    go :: [Integer] -> [Expr r] -> Eval r [Integer]
    go acc [] = pure $ reverse acc
    go acc (LInt x:xs) = go (x:acc) xs
    go _ (e:_) = typeError name "integer" e

iadd :: Builtin r
iadd _ args = asInts "+" args <&> LInt . foldl' (+) 0

-- unary should be negation
isub :: Builtin r
isub _ args = asInts "-" args <&> \case
  [x] -> LInt (-x)
  xs -> LInt $ foldl1' (-) xs

imul :: Builtin r
imul _ args = asInts "*" args <&> LInt . foldl' (*) 1

-- unary should be reciprocal
iidiv :: Builtin r
iidiv _ args = asInts "div" args >>= \case
  [x] -> pure $ LInt (1 `div` x)
  [] -> numArgsAtLeast "div" 1 []
  xs -> pure $ LInt $ foldl1' div xs

imod, iquot, irem :: Builtin r
imod _ args = asInts "mod" args >>= \case
  [] -> numArgsAtLeast "mod" 1 []
  xs -> pure $ LInt $ foldl1' mod xs
iquot _ args = asInts "quot" args >>= \case
  [] -> numArgsAtLeast "quot" 1 []
  xs -> pure $ LInt $ foldl1' quot xs
irem _ args = asInts "rem" args >>= \case
  [] -> numArgsAtLeast "rem" 1 []
  xs -> pure $ LInt $ foldl1' rem xs

comparison :: Symbol -> (Integer -> Integer -> Bool) -> [Expr r] -> Eval r (Expr r)
comparison name is args = asInts name args >>= \case
  [] -> numArgsAtLeast name 1 []
  xs -> pure $ LBool $ and $ zipWith is xs (tail xs)

ieq, ine, igt, ilt, ige, ile :: Builtin r
ieq _ = comparison "=" (==)
ine _ = comparison "/=" (/=)
igt _ = comparison ">" (>)
ilt _ = comparison "<" (<)
ige _ = comparison ">=" (>=)
ile _ = comparison "<=" (<=)

primLength :: Builtin r
primLength _ [LNull] = pure $ LInt 0
primLength _ [l@(LPair _ _)] = LInt . fromIntegral . NonEmpty.length <$> getList "length" l
primLength _ [LString xs] = pure $ LInt $ fromIntegral $ Text.length xs
primLength _ [x] = typeError "length" "sequence" x
primLength _ args = numArgs "length" 1 args

stringComparison :: Symbol -> (forall e. Ord e => e -> e -> Bool) -> [Expr r] -> Eval r (Expr r)
stringComparison _ cmp [LString x, LString y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LKeyword x, LKeyword y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LSymbol x, LSymbol y] = pure $ LBool (cmp x y)
stringComparison name _ [x, y] = evalError $ showt name <> ": invalid argument types " <> renderType x <> " and " <> renderType y
stringComparison name _ args = numArgs name 2 args

stringEq, stringGt, stringLt, stringGe, stringLe :: Builtin r
stringEq _ = stringComparison "string=" (==)
stringGt _ = stringComparison "string>" (>)
stringLt _ = stringComparison "string<" (<)
stringGe _ = stringComparison "string>=" (<=)
stringLe _ = stringComparison "string<=" (<=)

stringAppend :: Builtin r
stringAppend _ args = LString . Text.concat <$> traverse (getString "string-append") args

stringToSymbol :: Builtin r
stringToSymbol _ [x] = LSymbol . Symbol . mk <$> getString "string->symbol" x
stringToSymbol _ args = numArgs "string->symbol" 1 args

symbolToString :: Builtin r
symbolToString _ [x] = do
  Symbol s <- getSymbol "symbol->string" x
  pure $ LString $ foldedCase s
symbolToString _ args = numArgs "symbol->string" 1 args

equal :: Builtin r
equal _ [x, y] = pure $ LBool $ x == y
equal _ args = numArgs "equal?" 2 args

primGensym :: Builtin r
primGensym _ [] = LSymbol <$> liftIO genSym
primGensym _ args = numArgs "gensym" 0 args

primPrint :: Builtin r
primPrint _ [LString s] = liftIO (Text.IO.putStrLn s) $> LInert
primPrint _ [e] = liftIO (print e) $> LInert
primPrint _ args = numArgs "print" 1 args

primWrite :: Builtin r
primWrite _ [LString s] = liftIO (Text.IO.putStr s) $> LInert
primWrite _ [e] = liftIO (Text.IO.putStr (showt e)) $> LInert
primWrite _ args = numArgs "write" 1 args

primDisplay :: Builtin r
primDisplay _ [e] = liftIO (print e) $> LInert
primDisplay _ args = numArgs "display" 1 args

load :: Builtin r
load env [LString path] = do
  contents <- liftIO (Text.IO.readFile (Text.unpack path))
  evalFile env contents
load _ [e] = typeError "load" "string as path" e
load _ args = numArgs "load" 1 args

getDataFilePath :: Builtin r
getDataFilePath _ [LString name] = do
  path <- liftIO $ getDataFileName $ Text.unpack name
  pure $ LString $ Text.pack path
getDataFilePath _ [e] = typeError "get-data-file-path" "string as path" e
getDataFilePath _ args = numArgs "get-data-file-path" 1 args
