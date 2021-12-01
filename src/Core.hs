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
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))

import Errors
import Parser (parseFile)
import Types

nil :: Expr
nil = LList []

wrap :: Combiner -> Eval Combiner
wrap c = pure $ ApplicativeCombiner c

unwrap :: Combiner -> Eval Combiner
unwrap (ApplicativeCombiner c) = pure c
unwrap x = evalError $ "unwrap: not an applicative: " <> showt x

mkBindings :: [(Symbol, Expr)] -> Eval (Map Symbol (IORef Expr))
mkBindings pairs = liftIO $ do
  Map.fromList <$> traverse (\(x, y) -> newIORef y <&> (x,)) pairs

evalFile :: Environment -> Text -> Eval Expr
evalFile env contents =
  case parseFile contents of
    Right res -> progn env res
    Left e -> evalError $ "load: parse error: " <> Text.pack e

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

mkVau :: Binder -> Expr -> [Expr] -> Eval Combiner
mkVau dynamicEnvName params body = do
  let
    toError :: Text -> Eval a
    toError e = evalError $ "$vau" <> ": " <> e

    parseArgs :: Expr -> Eval ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr)
    parseArgs (LList ps) = mainArgs [] ps
    parseArgs t = toError $ "invalid argument list: expected list, but got " <> renderType t
    -- Parse the main arguments from the parameter list
    mainArgs :: [Binder] -> [Expr] -> Eval ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr)
    mainArgs ma []                         = pure (reverse ma, [], Nothing, mempty)
    mainArgs ma (LSymbol "&optional":spec) = optionalArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&rest":spec)     = restArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&key":spec)      = keyArgs (reverse ma, [], Nothing, mempty) spec
    mainArgs ma (LSymbol s:xs)             = mainArgs (NamedBinder s:ma) xs
    mainArgs ma (LIgnore:xs)               = mainArgs (IgnoreBinder:ma) xs
    mainArgs _  (x:_)                      = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the optional arguments from the parameter list
    optionalArgs :: ([Binder], [(Binder, Expr)]) -> [Expr] -> Eval ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr)
    optionalArgs (ma, oa) []                        = pure (ma, reverse oa, Nothing, mempty)
    optionalArgs _        (LSymbol "&optional":_)   = toError "&optional not allowed here"
    optionalArgs (ma, oa) (LSymbol "&rest":spec)    = restArgs (ma, reverse oa) spec
    optionalArgs (ma, oa) (LSymbol "&key":spec)     = keyArgs (ma, reverse oa, Nothing, mempty) spec
    optionalArgs (ma, oa) (LSymbol s:xs)            = optionalArgs (ma, (NamedBinder s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s]:xs)    = optionalArgs (ma, (NamedBinder s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s, v]:xs) = do env <- ask; v' <- eval env v; optionalArgs (ma, (NamedBinder s, v'):oa) xs
    optionalArgs (ma, oa) (LIgnore:xs)              = optionalArgs (ma, (IgnoreBinder, nil):oa) xs
    optionalArgs (ma, oa) (LList [LIgnore]:xs)      = optionalArgs (ma, (IgnoreBinder, nil):oa) xs
    optionalArgs (ma, oa) (LList [LIgnore, v]:xs)   = do env <- ask; v' <- eval env v; optionalArgs (ma, (IgnoreBinder, v'):oa) xs
    optionalArgs _        (x:_)                     = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the rest argument from the parameter list
    restArgs :: ([Binder], [(Binder, Expr)]) -> [Expr] -> Eval ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr)
    restArgs (ma, oa) []                               = pure (ma, oa, Nothing, mempty)
    restArgs _        [LSymbol "&optional"]            = toError "&optional not allowed here"
    restArgs _        [LSymbol "&rest"]                = toError "&rest not allowed here"
    restArgs (ma, oa) [LSymbol s]                      = pure (ma, oa, Just (NamedBinder s), mempty)
    restArgs (ma, oa) [LIgnore]                        = pure (ma, oa, Just IgnoreBinder, mempty)
    restArgs (ma, oa) (LSymbol s:LSymbol "&key":rest)  = keyArgs (ma, oa, Just (NamedBinder s), mempty) rest
    restArgs (ma, oa) (LIgnore:LSymbol "&key":rest)    = keyArgs (ma, oa, Just IgnoreBinder, mempty) rest
    restArgs _        (LSymbol _:x:_)                  = toError $ "unexpected extra argument after rest parameter: " <> showt x
    restArgs _        (x:_)                            = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the key arguments from the parameter list
    keyArgs :: ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr) -> [Expr] -> Eval ([Binder], [(Binder, Expr)], Maybe Binder, Map Symbol Expr)
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
  pure $ OperativeCombiner fun

-- Evaluate a list of expressions and return the value of the final expression
progn :: Environment -> [Expr] -> Eval Expr
progn env = go
  where
    go [] = pure nil
    go [x] = eval env x
    go (x:y) = eval env x *> go y

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
combine env (OperativeCombiner c) args = operate env c args
combine env (ApplicativeCombiner c) args = traverse (eval env) args >>= combine env c

operate :: Environment -> Fun -> [Expr] -> Eval Expr
operate env c args =
  case c of
    BuiltinFun f -> inEnvironment env $ f args
    UserFun Closure{..} -> do
      let
        name = "<combiner>"
        len = length closureParams
        argsError =
          if isJust closureRest
          then numArgsAtLeast name len args
          else numArgs name len args
        matchParams :: [Binder] -> [(Binder, Expr)] -> Maybe Binder -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams ps os r ks as = reverse <$> matchParams' [] ps os r ks as
        matchParams' :: [(Symbol, Expr)] -> [Binder] -> [(Binder, Expr)] -> Maybe Binder -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
        matchParams' bs (IgnoreBinder:ms)  os                      r        ks (_:as) = matchParams' bs ms os r ks as
        matchParams' bs (NamedBinder m:ms) os                      r        ks (a:as) = matchParams' ((m,a):bs) ms os r ks as
        matchParams' _  (_:_)              _                       _        _  []     = argsError -- not enough args
        matchParams' bs []                 ((NamedBinder o, _):os) r        ks (a:as) = matchParams' ((o,a):bs) [] os r ks as
        matchParams' bs []                 ((IgnoreBinder, _):os)  r        ks (_:as) = matchParams' bs [] os r ks as
        matchParams' bs []                 ((NamedBinder o, v):os) r        ks []     = matchParams' ((o, v):bs) [] os r ks []
        matchParams' bs []                 ((IgnoreBinder, _):os)  r        ks []     = matchParams' bs [] os r ks []
        matchParams' bs []                 []                      (Just IgnoreBinder) ks as
          | null ks = pure bs
          | otherwise = (bs ++) <$> matchParamsKeywords ks as
        matchParams' bs []                 []                      (Just (NamedBinder r)) ks as
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
        dynamicEnvBinding =
          case closureDynamicEnv of
            IgnoreBinder -> Nothing
            NamedBinder n -> Just (n, LEnv env)
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
