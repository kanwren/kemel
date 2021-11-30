{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import TextShow (TextShow(..))

import Errors
import Parser (parseFile)
import Types

nil :: Expr
nil = LList []

mkBindings :: [(Symbol, Expr)] -> Eval (Map Symbol (IORef Expr))
mkBindings pairs = liftIO $ do
  Map.fromList <$> traverse (\(x, y) -> newIORef y <&> (x,)) pairs

evalFile :: Environment -> String -> Eval Expr
evalFile env contents =
  case parseFile contents of
    Right res -> progn env res
    Left e -> evalError $ "load: parse error: " <> Text.pack e

hasVar :: Symbol -> Environment -> Eval Bool
hasVar i (Environment envVar) = do
  mapping <- liftIO $ readIORef envVar
  pure $ i `Map.member` mapping

lookupVar :: Symbol -> Environment -> Eval Expr
lookupVar i (Environment envVar) = do
  mapping <- liftIO $ readIORef envVar
  case mapping Map.!? i of
    Nothing -> evalError $ "variable not in scope: " <> showt i
    Just x  -> liftIO $ readIORef x

defineVar :: Symbol -> Expr -> Environment -> Eval ()
defineVar i val (Environment envVar) = do
  mapping <- liftIO $ readIORef envVar
  case mapping Map.!? i of
    Nothing -> do
      ref <- liftIO $ newIORef val
      liftIO $ writeIORef envVar $ Map.insert i ref mapping
    Just ref -> liftIO $ writeIORef ref val

mkVau :: Maybe Symbol -> Int -> [Expr] -> Eval Closure
mkVau dynamicEnvName n args = do
  env <- ask
  case args of
    (params:body) -> do
      (mainParams, rest) <- parseArgs params
      pure Closure
        { closureParams = mainParams
        , closureRest = rest
        , closureBody = body
        , closureStaticEnv = env
        , closureDynamicEnv = dynamicEnvName
        }
    -- if name is given, it was a parameter, so count it in the list
    _ -> numArgsAtLeast "$vau" n args
  where
    toError :: Text -> Eval a
    toError e = evalError $ "$vau" <> ": " <> e
    toSymbol :: Expr -> Eval Symbol
    toSymbol (LSymbol s) = pure s
    toSymbol x = toError $ "invalid argument list: invalid parameter " <> showt x
    parseArgs :: Expr -> Eval ([Symbol], Maybe Symbol)
    parseArgs (LList params) = (,Nothing) <$> traverse toSymbol params
    parseArgs (LDottedList params rest) = (,) <$> traverse toSymbol (NonEmpty.toList params) <*> (Just <$> toSymbol rest)
    parseArgs x = toError $ "invalid argument list: " <> showt x

typep :: Symbol -> Expr -> Expr -> Eval Bool
typep name v = go
  where
    go (LSymbol s) =
      case symbolToTypePred s of
        Just p  -> pure $ p v
        Nothing -> evalError $ showt name <> ": invalid type specifier"
    go (LList (LSymbol "and":spec)) = and <$> traverse go spec
    go (LList (LSymbol "or":spec)) = or <$> traverse go spec
    go (LList (LSymbol "not":spec)) =
      case spec of
        [p] -> not <$> go p
        _ -> evalError $ showt name <> ": expected exactly 1 argument to not, but got " <> showt (length spec)
    go (LList (LSymbol "integer":spec)) =
      case v of
        LInt n ->
          case spec of
            [LInt lower] -> pure $ lower <= n
            [LList [LInt lower]] -> pure $ lower <= n
            [LInt lower, LInt upper] -> pure $ lower <= n && n <= upper
            _ -> evalError $ showt name <> ": invalid type specifier: invalid arguments to predicate integer"
        _ -> pure False
    go _ = evalError $ showt name <> ": invalid type specifier"

-- Evaluate a list of expressions and return the value of the final expression
progn :: Environment -> [Expr] -> Eval Expr
progn _   [] = pure nil
progn env [x] = eval env x
progn env (x:y) = eval env x *> progn env y

buildTagTable :: [Expr] -> Eval (Map TagName Int, Vector Expr)
buildTagTable = fmap collect . foldlM go (0, mempty, mempty)
  where
    collect (_, tagTable, exprs) = (tagTable, Vector.fromList (reverse exprs))
    go :: (Int, Map TagName Int, [Expr]) -> Expr -> Eval (Int, Map TagName Int, [Expr])
    go (i, tagTable, exprs) = \case
      -- normal symbols
      LSymbol sym -> pure (i, Map.insert (TagSymbol sym) i tagTable, exprs)
      LKeyword b -> pure (i, Map.insert (TagKeyword b) i tagTable, exprs)
      LInt n -> pure (i, Map.insert (TagInt n) i tagTable, exprs)
      -- NIL symbol
      LList [] -> pure (i, Map.insert (TagSymbol "nil") i tagTable, exprs)
      -- lists
      l@(LList _) -> pure (i + 1, tagTable, l:exprs)
      l@(LDottedList _ _) -> pure (i + 1, tagTable, l:exprs)
      e -> evalError $ "tagbody: invalid tag or form type: " <> renderType e

block :: Symbol -> Eval Expr -> Eval Expr
block blockName a =
  a `catchError` \case
    ReturnFrom target val
      | blockName == target -> pure val
      -- NOTE: non-matching block names should bubble up
    e -> throwError e

eval :: Environment -> Expr -> Eval Expr
eval env (LSymbol sym) = lookupVar sym env
eval env (LDottedList xs _) = eval env (LList (NonEmpty.toList xs))
eval _ (LList []) = pure nil
eval env (LList (f:args)) =
  case f of
    -- NOTE: this builtin is needed to support quoting; a quoted expression
    -- `*x*` is literally turned into `(quote *x*)`, which should result in *x*
    -- without evaluating it. The renderer simply has a special case for lists
    -- of the form `(list 'quote x)`.
    _ -> eval env f >>= \case
      LCombiner c -> combine env c args
      e -> evalError $ "expected combiner in call: " <> showt e
-- everything other than a list and a symbol is a self-evaluating expression
eval _ f = pure f

combine :: Environment -> Combiner -> [Expr] -> Eval Expr
combine env c@(Combiner { combinerType }) args =
  case combinerType of
    OperativeCombiner -> operate env c args
    ApplicativeCombiner -> traverse (eval env) args >>= combine env (unwrap c)

operate :: Environment -> Combiner -> [Expr] -> Eval Expr
operate env c args =
  case combinerFun c of
    BuiltinFun f -> f args
    UserFun Closure{..} -> do
      let
        name = "<combiner>"
        len = length closureParams
        argsError =
          if isJust closureRest
          then numArgsAtLeast name len args
          else numArgs name len args
        matchParams :: [Symbol] -> Maybe Symbol -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams ps r as = reverse <$> matchParams' [] ps r as
        matchParams' :: [(Symbol, Expr)] -> [Symbol] -> Maybe Symbol -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams' bs (m:ms) r        (a:as) = matchParams' ((m,a):bs) ms r as
        matchParams' _  (_:_)  _        []     = argsError -- not enough args
        matchParams' bs []     (Just r) as     = pure $ (r, LList as):bs
        matchParams' bs []     Nothing  []     = pure bs
        matchParams' _  []     Nothing  (_:_)  = argsError -- too many args
        dynamicEnvBinding :: Maybe (Symbol, Expr)
        dynamicEnvBinding = (,LEnv env) <$> closureDynamicEnv
      paramBinds <- matchParams closureParams closureRest args
      binds <- mkBindings $ maybe paramBinds (:paramBinds) dynamicEnvBinding
      inEnvironment closureStaticEnv $ withLocalBindings binds $ do
        progn env closureBody `catchError` \case
          -- all (return-from)s need to be caught, or else we could bubble out of
          -- the current function! this is contrasted with `block`, which should
          -- let any (return-from)s with a different label to bubble up
          --
          -- TODO: functions should automatically get a block name
          ReturnFrom blockName _ -> evalError $ showt name <> ": error returning from block " <> showt (showt blockName) <> ": no such block in scope"
          TagGo tagName -> evalError $ showt name <> ": error going to tag " <> renderTagName tagName <> ": no such tag in scope"
          e -> throwError e
