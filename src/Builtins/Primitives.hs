{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Builtins.Primitives (builtinPrimitives) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.Bifunctor (second, first)
import Data.Functor (($>), (<&>))
import Data.List (foldl', foldl1')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit qualified as Exit
import TextShow (TextShow(..))

import Errors
import Core (evalFile)
import Types
import Builtins.Utils (builtinApp, Builtin)
import Data.IORef (newIORef)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = first (x:) <$> unsnoc xs

builtinPrimitives :: [(Symbol, Expr)]
builtinPrimitives = fmap (second builtinApp)
  [ ("make-new-environment", makeNewEnvironment)
  , ("cons", cons)
  , ("append", append) -- TODO: remove this
  , ("type-of", typeOf)
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
  , ("equal", equal)
  , ("length", primLength)
  , ("string=", stringEq)
  , ("string>", stringGt)
  , ("string<", stringLt)
  , ("string>=", stringGe)
  , ("string<=", stringLe)
  , ("gensym", primGensym)
  , ("print", printExpr)
  , ("load", load)
  , ("exit", exit)
  ]

makeNewEnvironment :: Builtin
makeNewEnvironment [] = LEnv . Environment <$> liftIO (newIORef mempty)
makeNewEnvironment args = numArgs "make-new-environment" 0 args

cons :: Builtin
cons [x, LList y] = pure $ LList (x:y)
cons [x, LDottedList (y :| ys) z] = pure $ LDottedList (x :| (y : ys)) z
cons [x, y] = pure $ LDottedList (x :| []) y
cons args = numArgs "cons" 2 args

append :: Builtin
append xs =
  case unsnoc xs of
    Nothing -> pure $ LList []
    Just (_, LList _) -> LList . concat <$> traverse getList xs
    Just (ys, z) -> do
      res <- concat <$> traverse getList ys
      case NonEmpty.nonEmpty res of
        Nothing -> pure z
        Just ys' -> pure $ LDottedList ys' z
  where
    getList (LList l) = pure l
    getList _ = evalError "append: expected list"

typeOf :: Builtin
typeOf [v] = pure $ LSymbol $ typeToSymbol v
typeOf args = numArgs "type-of" 1 args

asInts :: Symbol -> [Expr] -> Eval [Integer]
asInts name = go []
  where
    go acc [] = pure $ reverse acc
    go acc (LInt x:xs) = go (x:acc) xs
    go _ (e:_) = evalError $ showt name <> ": expected integer, but got " <> renderType e

iadd :: Builtin
iadd args = asInts "+" args <&> LInt . foldl' (+) 0

-- unary should be negation
isub :: Builtin
isub args = asInts "-" args <&> \case
  [x] -> LInt (-x)
  xs -> LInt $ foldl1' (-) xs

imul :: Builtin
imul args = asInts "*" args <&> LInt . foldl' (*) 1

-- unary should be reciprocal
iidiv :: Builtin
iidiv args = asInts "div" args >>= \case
  [x] -> pure $ LInt (1 `div` x)
  [] -> numArgsAtLeast "div" 1 []
  xs -> pure $ LInt $ foldl1' div xs

imod, iquot, irem :: Builtin
imod args = asInts "mod" args >>= \case
  [] -> numArgsAtLeast "mod" 1 []
  xs -> pure $ LInt $ foldl1' mod xs
iquot args = asInts "quot" args >>= \case
  [] -> numArgsAtLeast "quot" 1 []
  xs -> pure $ LInt $ foldl1' quot xs
irem args = asInts "rem" args >>= \case
  [] -> numArgsAtLeast "rem" 1 []
  xs -> pure $ LInt $ foldl1' rem xs

comparison :: Symbol -> (Integer -> Integer -> Bool) -> Builtin
comparison name is args = asInts name args >>= \case
  [] -> numArgsAtLeast name 1 []
  xs -> pure $ LBool $ and $ zipWith is xs (tail xs)

ieq, ine, igt, ilt, ige, ile :: Builtin
ieq = comparison "=" (==)
ine = comparison "/=" (/=)
igt = comparison ">" (>)
ilt = comparison "<" (<)
ige = comparison ">=" (>=)
ile = comparison "<=" (<=)

primLength :: Builtin
primLength [LList xs] = pure $ LInt $ fromIntegral $ length xs
primLength [LString xs] = pure $ LInt $ fromIntegral $ Text.length xs
primLength [_] = evalError "length: expected a sequence"
primLength args = numArgs "length" 1 args

stringComparison :: Symbol -> (forall e. Ord e => e -> e -> Bool) -> Builtin
stringComparison _ cmp [LString x, LString y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LKeyword x, LKeyword y] = pure $ LBool (cmp x y)
stringComparison _ cmp [LSymbol x, LSymbol y] = pure $ LBool (cmp x y)
stringComparison name _ [x, y] = evalError $ showt name <> ": invalid argument types " <> renderType x <> " and " <> renderType y
stringComparison name _ args = numArgs name 2 args

stringEq, stringGt, stringLt, stringGe, stringLe :: Builtin
stringEq = stringComparison "string=" (==)
stringGt = stringComparison "string>" (>)
stringLt = stringComparison "string<" (<)
stringGe = stringComparison "string>=" (<=)
stringLe = stringComparison "string<=" (<=)

equal :: Builtin
equal args =
  case args of
    [x, y] -> LBool <$> equal' x y
    _ -> numArgs "equal" 2 args
  where
    equal' :: Expr -> Expr -> Eval Bool
    equal' (LInt x) (LInt y) = pure (x == y)
    equal' (LBool x) (LBool y) = pure (x == y)
    equal' (LKeyword x) (LKeyword y) = pure (x == y)
    equal' (LString x) (LString y) = pure (x == y)
    equal' (LSymbol x) (LSymbol y) = pure (x == y)
    equal' (LList x) (LList y) =
      if length x /= length y
      then pure False
      else and <$> zipWithM equal' x y
    equal' (LDottedList x x') (LDottedList y y') =
      if length x /= length y
      then pure False
      else (&&) <$> (and <$> zipWithM equal' (NonEmpty.toList x) (NonEmpty.toList y)) <*> equal' x' y'
    equal' (LCombiner _) (LCombiner _) = pure False
    equal' x y = evalError $ "equal: incompatible types " <> renderType x <> " and " <> renderType y

primGensym :: Builtin
primGensym [] = LSymbol <$> genSym
primGensym args = numArgs "gensym" 0 args

printExpr :: Builtin
printExpr [e] = liftIO (print e) $> e
printExpr args = numArgs "print" 1 args

load :: Builtin
load [LString path] = do
  contents <- liftIO (Text.IO.readFile (Text.unpack path))
  env <- ask
  evalFile env contents
load [e] = evalError $ "load: expected string as path, but got " <> renderType e
load args = numArgs "load" 1 args

exit :: Builtin
exit [] = liftIO Exit.exitSuccess
exit [LInt n] = liftIO $ Exit.exitWith $ Exit.ExitFailure (fromIntegral n)
exit args = numArgsBound "exit" (0, 1) args

