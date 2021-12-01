{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Except ( ExceptT(ExceptT), MonadError )
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..), MonadReader, local, asks)
import Control.Monad.State (MonadState, StateT(..), state)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.Default (Default(..))
import Data.IORef (IORef, readIORef, newIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Ratio qualified as Ratio
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))
import TextShow qualified (fromText, unwordsB, FromTextShow(..))

data Symbol
  = SimpleSymbol (CI Text)
  | ArbSymbol Text
  deriving (Eq, Ord)

instance IsString Symbol where
  fromString = SimpleSymbol . fromString

instance TextShow Symbol where
  showb = \case
    SimpleSymbol s -> TextShow.fromText (foldedCase s)
    ArbSymbol s -> "|" <> TextShow.fromText (Text.replace "|" "\\|" s) <> "|"

newtype Keyword = Keyword { getKeyword :: Symbol }
  deriving (Eq, Ord)

instance TextShow Keyword where
  showb (Keyword s) = ":" <> showb s

data Closure = Closure
  { closureParams :: [Symbol]
  , closureOptionalParams :: [(Symbol, Expr)]
  , closureRest :: Maybe Symbol
  , closureKeywordParams :: Map Symbol Expr
  , closureDynamicEnv :: Maybe Symbol
  , closureStaticEnv :: Environment
  , closureBody :: [Expr]
  }

data CombinerType = OperativeCombiner | ApplicativeCombiner
-- TODO: rename this type
data Fun = BuiltinFun ([Expr] -> Eval Expr) | UserFun !Closure
data Combiner = Combiner
  { combinerType :: CombinerType
  , combinerFun  :: Fun
  }

wrap, unwrap :: Combiner -> Combiner
wrap c = c { combinerType = ApplicativeCombiner }
unwrap c = c { combinerType = OperativeCombiner }

data Expr
  = LInt Integer
  | LBool Bool
  | LKeyword Keyword
  | LString Text
  | LSymbol Symbol
  | LEnv Environment
  | LList [Expr]
  | LDottedList (NonEmpty Expr) Expr
  | LCombiner Combiner
  deriving Show via (TextShow.FromTextShow Expr)

renderType :: Expr -> Text
renderType = showt . typeToSymbol

typeToSymbol :: Expr -> Symbol
typeToSymbol = SimpleSymbol . \case
  LInt _ -> "integer"
  LBool _ -> "bool"
  LKeyword _ -> "keyword"
  LString _ -> "string"
  LSymbol _ -> "symbol"
  LEnv _ -> "environment"
  LList [] -> "null"
  LList _ -> "cons"
  LDottedList _ _ -> "cons"
  LCombiner _ -> "combiner"

symbolToTypePred :: Symbol -> Maybe (Expr -> Bool)
symbolToTypePred = \case
  "number" -> pure $ \case LInt _ -> True; _ -> False
  "integer" -> pure $ \case LInt _ -> True; _ -> False
  "bool" -> pure $ \case LBool _ -> True; _ -> False
  "keyword" -> pure $ \case LKeyword _ -> True; _ -> False
  "string" -> pure $ \case LString _ -> True; _ -> False
  "symbol" -> pure $ \case LSymbol _ -> True; _ -> False
  "environment" -> pure $ \case LEnv _ -> True; _ -> False
  "null" -> pure $ \case LList [] -> True; _ -> False
  "list" -> pure $ \case LList _ -> True; LDottedList _ _ -> True; _ -> False
  "cons" -> pure $ \case LDottedList _ _ -> True; LList (_:_) -> True; _ -> False
  "combiner" -> pure $ \case LCombiner{} -> True; _ -> False
  _ -> Nothing

renderRatio :: Rational -> Text
renderRatio n = showt num <> "/" <> showt den
  where (num, den) = (Ratio.numerator n, Ratio.denominator n)

instance TextShow Expr where
  showb = \case
    LInt n -> showb n
    LBool False -> "#f"
    LBool True -> "#t"
    LKeyword kw -> showb kw
    LString s -> showb s
    LSymbol s -> showb s
    LEnv _ -> "<environment>"
    LList [LSymbol "quote", x] -> "'" <> showb x
    LList [] -> "nil"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb (NonEmpty.toList xs)) <> " . " <> showb x <> ")"
    LCombiner _ -> "<combiner>"

-- Evaluation context (scopes)

newtype Error = Error { getError :: Text }

data TagName
  = TagInt Integer
  | TagRatio Rational
  | TagSymbol Symbol
  | TagKeyword Keyword
  deriving (Eq, Ord)

renderTagName :: TagName -> Text
renderTagName = \case
  TagInt n -> showt n
  TagRatio n -> renderRatio n
  TagSymbol sym -> showt sym
  TagKeyword kw -> showt kw

data Bubble
  = ReturnFrom Symbol Expr
  | TagGo TagName
  | EvalError Error

newtype Environment = Environment { getEnvironment :: IORef (Map Symbol (IORef Expr)) }

-- Symbol generation

newtype SymbolGenerator = SymbolGenerator { getSymGen :: Int }

nextSym :: SymbolGenerator -> (Symbol, SymbolGenerator)
nextSym (SymbolGenerator n) = (SimpleSymbol (mk ("#:g" <> showt n)), SymbolGenerator (n + 1))

instance Default SymbolGenerator where
  def = SymbolGenerator 0

class SymGen m where
  genSym :: m Symbol

instance MonadState SymbolGenerator m => SymGen m where
  genSym = state nextSym

-- Eval

newtype Eval a = Eval { runEval :: Environment -> SymbolGenerator -> IO (Either Bubble a, SymbolGenerator) }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader Environment, MonadError Bubble
    , MonadState SymbolGenerator
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )
    via ReaderT Environment (ExceptT Bubble (StateT SymbolGenerator IO))

inEnvironment :: Environment -> Eval a -> Eval a
inEnvironment ctx' = local (const ctx')

withLocalBindings :: Map Symbol (IORef Expr) -> Eval a -> Eval a
withLocalBindings bindings act = do
  ctx <- liftIO . readIORef =<< asks getEnvironment
  ctx' <- liftIO $ newIORef $ bindings <> ctx
  let newEnv = Environment ctx'
  local (const newEnv) act
