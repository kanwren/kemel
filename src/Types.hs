{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Types where

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Except ( ExceptT(ExceptT), MonadError )
import Control.Monad.IO.Class
import Control.Monad.State (MonadState, StateT(..), state)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.Default (Default(..))
import Data.IORef (IORef, newIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))
import TextShow qualified (fromText, unwordsB, FromTextShow(..))

-- | The data type of all variable names
data Symbol
  = SimpleSymbol (CI Text)
  | ArbSymbol Text
  deriving stock (Eq, Ord)

instance IsString Symbol where
  fromString = SimpleSymbol . fromString

instance TextShow Symbol where
  showb = \case
    SimpleSymbol s -> TextShow.fromText (foldedCase s)
    ArbSymbol s -> "|" <> TextShow.fromText (Text.replace "|" "\\|" s) <> "|"

newtype Keyword = Keyword { getKeyword :: Symbol }
  deriving stock (Eq, Ord)

instance TextShow Keyword where
  showb (Keyword s) = ":" <> showb s

-- | Specifies whether to bind a variable when calling an operator, and if so,
-- what name it should get. For example, `($lambda (#ignore) ...)` will take one
-- parameter, but will not bind it to any names.
data Binder
  = IgnoreBinder
  | NamedBinder Symbol

data ParamTree
  = BoundParam Binder
  | ParamList [ParamTree]
  | ParamDottedList (NonEmpty ParamTree) Binder -- last part of dotted list can't be a list

-- | The definition of a user-defined vau operative. Holds the parameter
-- specification, vau body, and the static environment closed over when the vau
-- is created.
data Closure = Closure
  { closureParams :: ParamTree
  , closureDynamicEnv :: Binder
  , closureStaticEnv :: Environment
  , closureBody :: Expr
  }

type Builtin = Environment -> [Expr] -> Eval Expr

-- | A callable operative, which is either a builtin defined from Haskell (a
-- plain Haskell function) or a user-defined vau closure.
data Operative = BuiltinOp Builtin | UserOp !Closure

-- | A combiner at the head of a call is either _operative_ or _applicative_;
-- applicatives will first evaluate their arguments before calling the
-- underlying combiner.
data Combiner
  = OperativeCombiner Operative
  | ApplicativeCombiner Combiner

instance TextShow Combiner where
  showb = \case
    OperativeCombiner{} -> "<operative>"
    ApplicativeCombiner{} -> "<applicative>"

data Expr
  = LInert
  | LIgnore
  | LInt Integer
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
  LInert -> "inert"
  LIgnore -> "ignore"
  LInt _ -> "integer"
  LBool _ -> "bool"
  LKeyword _ -> "keyword"
  LString _ -> "string"
  LSymbol _ -> "symbol"
  LEnv _ -> "environment"
  LList [] -> "null"
  LList (_:_) -> "pair"
  LDottedList _ _ -> "pair"
  LCombiner (OperativeCombiner _) -> "operative"
  LCombiner (ApplicativeCombiner _) -> "applicative"

symbolToTypePred :: Symbol -> Maybe (Expr -> Bool)
symbolToTypePred = \case
  "inert" -> pure $ \case LInert -> True; _ -> False
  "ignore" -> pure $ \case LIgnore -> True; _ -> False
  "number" -> pure $ \case LInt _ -> True; _ -> False
  "integer" -> pure $ \case LInt _ -> True; _ -> False
  "bool" -> pure $ \case LBool _ -> True; _ -> False
  "keyword" -> pure $ \case LKeyword _ -> True; _ -> False
  "string" -> pure $ \case LString _ -> True; _ -> False
  "symbol" -> pure $ \case LSymbol _ -> True; _ -> False
  "environment" -> pure $ \case LEnv _ -> True; _ -> False
  "null" -> pure $ \case LList [] -> True; _ -> False
  "list" -> pure $ \case LList _ -> True; LDottedList _ _ -> True; _ -> False
  "pair" -> pure $ \case LDottedList _ _ -> True; LList (_:_) -> True; _ -> False
  "combiner" -> pure $ \case LCombiner _ -> True; _ -> False
  "operative" -> pure $ \case LCombiner (OperativeCombiner _) -> True; _ -> False
  "applicative" -> pure $ \case LCombiner (ApplicativeCombiner _) -> True; _ -> False
  _ -> Nothing

instance TextShow Expr where
  showb = \case
    LInert -> "#inert"
    LIgnore -> "#ignore"
    LInt n -> showb n
    LBool False -> "#f"
    LBool True -> "#t"
    LKeyword kw -> showb kw
    LString s -> showb s
    LSymbol s -> showb s
    LEnv _ -> "<environment>"
    LList [LSymbol "$quote", x] -> "'" <> showb x
    LList [] -> "()"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb (NonEmpty.toList xs)) <> " . " <> showb x <> ")"
    LCombiner c -> showb c

-- Evaluation context (scopes)

-- | All computations that exceptionally bubble up. Currently, this is only
-- exceptions, but other forms of control flow may use this mechanism in the
-- future.
newtype Bubble = EvalError Text

-- | A handle to a mapping of variable names to values, along with any parent
-- environments.
--
-- Both the environment and the values in it must be wrapped in `IORef`s.
-- If the environment is not in an `IORef`, then a function `f` defined before a
-- function `g` would not be able to see the later addition of the new function,
-- despite the fact that both are in the global scope. If the values are not
-- wrapped in `IORef`s, then changes to variables captured in sub-environments
-- (for example, global variables captured from closures), would not be visible
-- to the rest of the global scope.
data Environment =
  Environment
    (IORef (Map Symbol (IORef Expr)))
    -- ^ The variable bindings in this environment
    [Environment]
    -- ^ The parent environments
  deriving stock (Eq)

newEnvironment :: [Environment] -> IO Environment
newEnvironment parents = do
  m <- newIORef mempty
  pure $ Environment m parents

newEnvironmentWith :: [(Symbol, Expr)] -> [Environment] -> IO Environment
newEnvironmentWith bindings parents = do
  vars <- traverse (\(x, y) -> (x,) <$> newIORef y) bindings
  m <- newIORef $ Map.fromList vars
  pure $ Environment m parents

-- Symbol generation

-- | The state which is used to produce new symbols for `gensym`.
newtype SymbolGenerator = SymbolGenerator { getSymGen :: Int }

instance Default SymbolGenerator where
  def = SymbolGenerator 0

-- | Generate a new symbol, modifying the symbol generator in the computation's
-- state.
genSym :: MonadState SymbolGenerator m => m Symbol
genSym = state nextSym
  where
    nextSym :: SymbolGenerator -> (Symbol, SymbolGenerator)
    nextSym (SymbolGenerator n) = (SimpleSymbol (mk ("#:g" <> showt n)), SymbolGenerator (n + 1))

-- Eval

-- | The monad for evaluating expressions.
newtype Eval a = Eval { runEval :: SymbolGenerator -> IO (Either Bubble a, SymbolGenerator) }
  deriving
    ( Functor, Applicative, Monad
    , MonadError Bubble
    , MonadState SymbolGenerator
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )
    via ExceptT Bubble (StateT SymbolGenerator IO)

runProgram :: Eval a -> IO (Either Bubble a)
runProgram program = fst <$> runEval program def
