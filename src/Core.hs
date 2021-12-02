{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
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

lookupVar :: Symbol -> Environment -> Eval (Maybe Expr)
lookupVar i (Environment envVar parents) = do
  mapping <- liftIO $ readIORef envVar
  case mapping Map.!? i of
    Nothing -> do
      let
        go [] = pure Nothing
        go (env:envs) = lookupVar i env >>= \case
          Just x -> pure $ Just x
          Nothing -> go envs
      go parents
    Just x  -> fmap Just $ liftIO $ readIORef x

defineVar :: Symbol -> Expr -> Environment -> Eval ()
defineVar i val (Environment envVar _) = do
  mapping <- liftIO $ readIORef envVar
  case mapping Map.!? i of
    Nothing -> do
      ref <- liftIO $ newIORef val
      liftIO $ writeIORef envVar $ Map.insert i ref mapping
    Just ref -> liftIO $ writeIORef ref val

parseParamTree :: Symbol -> Expr -> Eval ParamTree
parseParamTree name = go
  where
    single :: Expr -> Eval Binder
    single LIgnore = pure IgnoreBinder
    single (LSymbol s) = pure $ NamedBinder s
    single x = evalError $ showt name <> ": invalid paramtere tree: " <> showt x

    go (LList ps) = ParamList <$> traverse go ps
    go (LDottedList ps p) = ParamDottedList <$> traverseNE go ps <*> single p
      where
        traverseNE :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
        traverseNE f (x :| xs) = (:|) <$> f x <*> traverse f xs
    go x = BoundParam <$> single x

matchParams :: Symbol -> ParamTree -> Expr -> Eval [(Symbol, Expr)]
matchParams name tree args = do
    case runExcept $ execWriterT $ go tree args of
      Left e -> throwError $ EvalError e
      Right binds -> pure binds
  where
    bind :: MonadWriter [(Symbol, Expr)] m => Binder -> Expr -> m ()
    bind IgnoreBinder _ = pure ()
    bind (NamedBinder s) v = tell [(s, v)]

    zipLeftover :: [a] -> [b] -> ([(a, b)], Maybe (Either [a] [b]))
    zipLeftover [] [] = ([], Nothing)
    zipLeftover (x:xs) [] = ([], Just (Left (x:xs)))
    zipLeftover [] (y:ys) = ([], Just (Right (y:ys)))
    zipLeftover (x:xs) (y:ys) = let (res, lo) = zipLeftover xs ys in ((x, y):res, lo)

    toError :: Text -> WriterT [(Symbol, Expr)] (Except Text) ()
    toError e = throwError $ showt name <> ": " <> e

    go :: ParamTree -> Expr -> WriterT [(Symbol, Expr)] (Except Text) ()
    go (BoundParam b) p = bind b p
    go (ParamList bs) (LList ps) =
      case zipLeftover bs ps of
        (pairs, Nothing) -> traverse_ (uncurry go) pairs
        (_, Just (Left _)) -> toError "not enough values when unpacking list"
        (_, Just (Right _)) -> toError "too many values when unpacking list"
    go (ParamList _) (LDottedList _ _) = toError "attempted to unpack dotted list into proper list"
    go (ParamList _) x = toError $ "expected a list to unpack, but got " <> renderType x
    go (ParamDottedList bs b) (LList ps) =
      case zipLeftover (NonEmpty.toList bs) ps of
        (pairs, Nothing) -> traverse_ (uncurry go) pairs *> bind b (LList [])
        (_, Just (Left _)) -> toError "not enough values when unpacking list"
        (pairs, Just (Right leftover)) -> traverse_ (uncurry go) pairs *> bind b (LList leftover)
    go (ParamDottedList bs b) (LDottedList ps p) =
      case zipLeftover (NonEmpty.toList bs) (NonEmpty.toList ps) of
        (pairs, Nothing) -> traverse_ (uncurry go) pairs *> bind b p
        (_, Just (Left _)) -> toError "not enough values when unpacking list"
        (pairs, Just (Right leftover)) -> traverse_ (uncurry go) pairs *>
          case leftover of
            [] -> bind b p
            (x:xs) -> bind b (LDottedList (x:|xs) p)
    go (ParamDottedList _ _) x = toError $ "expected a list to unpack, but got " <> renderType x

mkVau :: Environment -> Binder -> Expr -> Expr -> Eval Combiner
mkVau staticEnv dynamicEnvName params body = do
  paramTree <- parseParamTree "$vau" params
  let
    closure = Closure
      { closureParams = paramTree
      , closureBody = body
      , closureStaticEnv = staticEnv
      , closureDynamicEnv = dynamicEnvName
      }
    fun = UserOp closure
  pure $ OperativeCombiner fun

-- Evaluate a list of expressions and return the value of the final expression
progn :: Environment -> [Expr] -> Eval Expr
progn env = go
  where
    go [] = pure LInert
    go [x] = eval env x
    go (x:y) = eval env x *> go y

eval :: Environment -> Expr -> Eval Expr
eval env (LSymbol sym)      = lookupVar sym env >>= \case
  Just x  -> pure x
  Nothing -> evalError $ "variable not in scope: " <> showt sym
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

operate :: Environment -> Operative -> [Expr] -> Eval Expr
operate env c args = do
  case c of
    BuiltinOp f -> f env args
    UserOp Closure{..} -> do
      let
        dynamicEnvBinding :: Maybe (Symbol, Expr)
        dynamicEnvBinding =
          case closureDynamicEnv of
            IgnoreBinder -> Nothing
            NamedBinder n -> Just (n, LEnv env)
      paramBinds <- matchParams "<combiner>" closureParams (LList args)
      let binds = maybe paramBinds (:paramBinds) dynamicEnvBinding
      env' <- liftIO $ newEnvironmentWith binds [closureStaticEnv]
      eval env' closureBody

evalFile :: Environment -> Text -> Eval Expr
evalFile env contents =
  case parseFile contents of
    Right res -> progn env res
    -- TODO: fix this error message
    Left e -> evalError $ "load: parse error: " <> Text.pack e
