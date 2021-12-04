{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty((:|)))
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
parseParamTree name = go
  where
    single LIgnore = pure IgnoreBinder
    single (LSymbol s) = pure $ NamedBinder s
    single x = evalError $ showt name <> ": invalid parameter tree: " <> showt x

    go (LList ps) = ParamList <$> traverse go ps
    go (LDottedList ps p) = ParamDottedList <$> traverseNE go ps <*> single p
      where
        traverseNE :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
        traverseNE f (x :| xs) = (:|) <$> f x <*> traverse f xs
    go x = BoundParam <$> single x

matchParams :: Symbol -> ParamTree -> Expr r -> Eval r [(Symbol, Expr r)]
matchParams name tree args = liftEither $ runExcept $ execWriterT $ go tree args
  where
    bind :: MonadWriter [(Symbol, Expr r)] m => Binder -> Expr r -> m ()
    bind IgnoreBinder _ = pure ()
    bind (NamedBinder s) v = tell [(s, v)]

    zipLeftover :: [a] -> [b] -> ([(a, b)], Maybe (Either [a] [b]))
    zipLeftover [] [] = ([], Nothing)
    zipLeftover (x:xs) [] = ([], Just (Left (x:xs)))
    zipLeftover [] (y:ys) = ([], Just (Right (y:ys)))
    zipLeftover (x:xs) (y:ys) = let (res, lo) = zipLeftover xs ys in ((x, y):res, lo)

    toError :: Text -> WriterT [(Symbol, Expr r)] (Except Error) ()
    toError e = evalError $ showt name <> ": " <> e <> "; " <> showt (renderTree tree) <> " <- " <> showt args
      where
        renderTree :: ParamTree -> Expr r
        renderTree (BoundParam IgnoreBinder) = LIgnore
        renderTree (BoundParam (NamedBinder s)) = LSymbol s
        renderTree (ParamList ps) = LList (fmap renderTree ps)
        renderTree (ParamDottedList ps p) = LDottedList (fmap renderTree ps) (renderTree (BoundParam p))

    go :: ParamTree -> Expr r -> WriterT [(Symbol, Expr r)] (Except Error) ()
    go (BoundParam b) p = bind b p
    go (ParamList bs) (LList ps) =
      case zipLeftover bs ps of
        (pairs, Nothing) -> traverse_ (uncurry go) pairs
        (_, Just (Left _)) -> toError "not enough values when unpacking list"
        (_, Just (Right _)) -> toError "too many values when unpacking list"
    go (ParamList _) (LDottedList _ _) = toError "attempted to unpack dotted list into proper list"
    go (ParamList _) x = typeError name "a list to unpack" x
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
    go (ParamDottedList _ _) x = typeError name "a list to unpack" x

-- Evaluate a list of expressions and return the value of the final expression
progn :: Environment r -> [Expr r] -> Eval r (Expr r)
progn env = go
  where
    go [] = pure LInert
    go [x] = eval env x
    go (x:y) = eval env x *> go y

eval :: Environment r -> Expr r -> Eval r (Expr r)
eval env (LSymbol sym)      = lookupVar env sym >>= \case
  Just x  -> pure x
  Nothing -> evalError $ "variable not in scope: " <> showt sym
eval env (LDottedList xs _) = eval env (LList (NonEmpty.toList xs))
eval _   (LList [])         = pure $ LList []
eval env l@(LList (f:args))   = eval env f >>= \case
  LCombiner c -> combine env c args
  e -> evalError $ "expected combiner in call, but got " <> renderType e <> ": " <> showt l
-- everything other than a list and a symbol is a self-evaluating expression
eval _   f                  = pure f

combine :: Environment r -> Combiner r -> [Expr r] -> Eval r (Expr r)
combine env (OperativeCombiner c) args = operate env c args
combine env (ApplicativeCombiner c) args = traverse (eval env) args >>= combine env c

operate :: Environment r -> Operative r -> [Expr r] -> Eval r (Expr r)
operate env c args = do
  case c of
    BuiltinOp f -> f env args
    UserOp Closure{..} -> do
      paramBinds <- matchParams "<combiner>" closureParams (LList args)
      let binds = maybe paramBinds (:paramBinds) $ do
            case closureDynamicEnv of
              IgnoreBinder -> Nothing
              NamedBinder n -> Just (n, LEnv env)
      env' <- liftIO $ newEnvironmentWith binds [closureStaticEnv]
      eval env' closureBody

evalFile :: Environment r -> Text -> Eval r (Expr r)
evalFile env contents =
  case parseFile contents of
    Right res -> progn env res
    -- TODO: fix this error message
    Left e -> evalError $ "load: parse error: " <> Text.pack e
