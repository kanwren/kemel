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

mkVau :: Maybe Symbol -> Expr -> [Expr] -> Eval Combiner
mkVau dynamicEnvName params body = do
  let
    toError :: Text -> Eval a
    toError e = evalError $ "$vau" <> ": " <> e

    parseArgs :: Expr -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    parseArgs (LList ps) = mainArgs [] ps
    parseArgs t = toError $ "invalid argument list: expected list, but got " <> renderType t
    -- Parse the main arguments from the parameter list
    mainArgs :: [Symbol] -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    mainArgs ma []                         = pure (reverse ma, [], Nothing, mempty)
    mainArgs ma (LSymbol "&optional":spec) = optionalArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&rest":spec)     = restArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&key":spec)      = keyArgs (reverse ma, [], Nothing, mempty) spec
    mainArgs ma (LSymbol s:xs)             = mainArgs (s:ma) xs
    mainArgs _  (x:_)                      = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the optional arguments from the parameter list
    optionalArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    optionalArgs (ma, oa) []                        = pure (ma, reverse oa, Nothing, mempty)
    optionalArgs _        (LSymbol "&optional":_)   = toError "&optional not allowed here"
    optionalArgs (ma, oa) (LSymbol "&rest":spec)    = restArgs (ma, reverse oa) spec
    optionalArgs (ma, oa) (LSymbol "&key":spec)     = keyArgs (ma, reverse oa, Nothing, mempty) spec
    optionalArgs (ma, oa) (LSymbol s:xs)            = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s]:xs)    = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s, v]:xs) = do env <- ask; v' <- eval env v; optionalArgs (ma, (s, v'):oa) xs
    optionalArgs _        (x:_)                     = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the rest argument from the parameter list
    restArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    restArgs (ma, oa) []                               = pure (ma, oa, Nothing, mempty)
    restArgs _        [LSymbol "&optional"]            = toError "&optional not allowed here"
    restArgs _        [LSymbol "&rest"]                = toError "&rest not allowed here"
    restArgs (ma, oa) [LSymbol s]                      = pure (ma, oa, Just s, mempty)
    restArgs (ma, oa) (LSymbol s:LSymbol "&key":rest)  = keyArgs (ma, oa, Just s, mempty) rest
    restArgs _        (LSymbol _:x:_)                  = toError $ "unexpected extra argument after rest parameter: " <> showt x
    restArgs _        (x:_)                            = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the key arguments from the parameter list
    keyArgs :: ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    keyArgs (ma, oa, r, ka) []                         = pure (ma, oa, r, ka)
    keyArgs _               (LSymbol "&optional":_)    = toError "&optional not allowed here"
    keyArgs _               (LSymbol "&rest":_)        = toError "&rest not allowed here"
    keyArgs _               (LSymbol "&key":_)         = toError "&key not allowed here"
    keyArgs (ma, oa, r, ka) (LSymbol s:xs)             = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s]:xs)     = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s, v]:xs)  = do env <- ask; v' <- eval env v; keyArgs (ma, oa, r, Map.insert s v' ka) xs
    keyArgs _               (x:_)                      = toError $ "invalid argument list: invalid parameter " <> showt x
  (mainParams, optionals, rest, keywordParams) <- parseArgs params
  env <- ask
  let
    closure = Closure
      { closureParams = mainParams
      , closureOptionalParams = optionals
      , closureRest = rest
      , closureKeywordParams = keywordParams
      , closureBody = body
      , closureStaticEnv = env
      , closureDynamicEnv = dynamicEnvName
      }
    fun = UserFun closure
    combiner = Combiner
      { combinerType = OperativeCombiner
      , combinerFun = fun
      }
  pure combiner

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
progn env = go
  where
    go [] = pure nil
    go [x] = eval env x
    go (x:y) = eval env x *> go y

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
eval env (LSymbol sym)      = lookupVar sym env
eval env (LDottedList xs _) = eval env (LList (NonEmpty.toList xs))
eval _   (LList [])         = pure nil
eval env (LList (f:args))   = eval env f >>= \case
  LCombiner c -> combine env c args
  e -> evalError $ "expected combiner in call: " <> showt e
-- everything other than a list and a symbol is a self-evaluating expression
eval _   f                  = pure f

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
        matchParams :: [Symbol] -> [(Symbol, Expr)] -> Maybe Symbol -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams ps os r ks as = reverse <$> matchParams' [] ps os r ks as
        matchParams' :: [(Symbol, Expr)] -> [Symbol] -> [(Symbol, Expr)] -> Maybe Symbol -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams' bs (m:ms) os          r        ks (a:as) = matchParams' ((m,a):bs) ms os r ks as
        matchParams' _  (_:_)  _           _        _  []     = argsError -- not enough args
        matchParams' bs []     ((o, _):os) r        ks (a:as) = matchParams' ((o,a):bs) [] os r ks as
        matchParams' bs []     ((o, v):os) r        ks []     = matchParams' ((o, v):bs) [] os r ks []
        matchParams' bs []     []          (Just r) ks as
          | null ks = pure $ (r, LList as):bs
          | otherwise = (((r, LList as):bs) ++) <$> matchParamsKeywords ks as
        matchParams' bs []     []          Nothing  ks as
          | not (null as) && null ks = argsError -- better error messages when no keyword args and too many parameters
          | otherwise = (bs ++) <$> matchParamsKeywords ks as
        matchParamsKeywords :: Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
        matchParamsKeywords = go
          where
            go res [] = pure $ Map.assocs res
            go res (LKeyword k@(Keyword s):v:rest)
              | s `Map.member` res = go (Map.insert s v res) rest
              | otherwise = evalError $ showt name <> ": unexpected keyword: " <> showt k
            go _ (LKeyword _:_) = evalError $ showt name <> ": expected value for keyword argument"
            go _ (x:_) = evalError $ showt name <> ": unexpected parameter in keyword arguments: " <> showt x
        dynamicEnvBinding :: Maybe (Symbol, Expr)
        dynamicEnvBinding = (,LEnv env) <$> closureDynamicEnv
      paramBinds <- matchParams closureParams closureOptionalParams closureRest closureKeywordParams args
      binds <- mkBindings $ maybe paramBinds (:paramBinds) dynamicEnvBinding
      inEnvironment closureStaticEnv $ withLocalBindings binds $ do
        env' <- ask
        progn env' closureBody `catchError` \case
          -- all (return-from)s need to be caught, or else we could bubble out of
          -- the current function! this is contrasted with `block`, which should
          -- let any (return-from)s with a different label to bubble up
          --
          -- TODO: functions should automatically get a block name
          ReturnFrom blockName _ -> evalError $ showt name <> ": error returning from block " <> showt (showt blockName) <> ": no such block in scope"
          TagGo tagName -> evalError $ showt name <> ": error going to tag " <> renderTagName tagName <> ": no such tag in scope"
          e -> throwError e
