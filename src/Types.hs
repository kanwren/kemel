{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
    Symbol(..), Keyword(..),
    Binder(..), ParamTree(..), Closure(..), Operative(..), Combiner(..), Builtin,
    Expr(..),
    renderType, typeToSymbol, symbolToTypePred,
    Encapsulation(..),
    Environment(..), newEnvironment, newEnvironmentWith,
    genSym,
    Eval(..), Error(..)
  ) where

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Except ( ExceptT(ExceptT), MonadError )
import Control.Monad.IO.Class
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.HashTable.IO qualified as HIO
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Unique (Unique)
import System.IO.Unsafe (unsafePerformIO)
import TextShow (TextShow(..))
import TextShow qualified (fromText, unwordsB, FromTextShow(..))

-- | The data type of all variable names
newtype Symbol = Symbol (CI Text)
  deriving newtype (Eq, Ord, IsString, Hashable)

instance TextShow Symbol where
  showb (Symbol s) = TextShow.fromText (foldedCase s)

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
  | ParamDottedList (NonEmpty ParamTree) Binder

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
data Combiner = OperativeCombiner Operative | ApplicativeCombiner Combiner

instance TextShow Combiner where
  showb = \case
    OperativeCombiner{} -> "<operative>"
    ApplicativeCombiner{} -> "<applicative>"

data Encapsulation = Encapsulation !Unique !(IORef Expr)
  deriving stock Eq

data Expr
  = LInert
  | LIgnore
  | LInt !Integer
  | LBool !Bool
  | LKeyword !Keyword
  | LString !Text
  | LSymbol !Symbol
  | LEncapsulation !Encapsulation
  | LEnv !Environment
  | LList ![Expr]
  | LDottedList !(NonEmpty Expr) !Expr
  | LCombiner !Combiner
  deriving Show via (TextShow.FromTextShow Expr)

renderType :: Expr -> Text
renderType = showt . typeToSymbol

typeToSymbol :: Expr -> Symbol
typeToSymbol = \case
  LInert -> "inert"
  LIgnore -> "ignore"
  LInt _ -> "integer"
  LBool _ -> "bool"
  LKeyword _ -> "keyword"
  LString _ -> "string"
  LSymbol _ -> "symbol"
  LEncapsulation _ -> "encapsulation"
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
    LEncapsulation _ -> "<encapsulation>"
    LEnv _ -> "<environment>"
    LList [] -> "()"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb (NonEmpty.toList xs)) <> " . " <> showb x <> ")"
    LCombiner c -> showb c

newtype Error = EvalError Text

-- | A handle to a mapping of variable names to values, along with any parent
-- environments.
data Environment =
  Environment
    (HIO.BasicHashTable Symbol Expr)
    -- ^ The variable bindings in this environment
    [Environment]
    -- ^ The parent environments

instance Eq Environment where
  _ == _ = False -- TODO: equate on STRefs

newEnvironment :: [Environment] -> IO Environment
newEnvironment parents = do
  m <- HIO.new
  pure $ Environment m parents

newEnvironmentWith :: [(Symbol, Expr)] -> [Environment] -> IO Environment
newEnvironmentWith bindings parents = do
  m <- HIO.fromList bindings
  pure $ Environment m parents

symbolSource :: IORef Integer
symbolSource = unsafePerformIO (newIORef 0)
{-# NOINLINE symbolSource #-}

-- | Generate a new symbol, modifying the symbol generator in the computation's
-- state.
genSym :: IO Symbol
genSym = do
  n <- atomicModifyIORef' symbolSource $ \cur ->
    let new = cur + 1
    in (cur, new)
  pure $ Symbol $ mk $ "#:g" <> showt n

-- | The monad for evaluating expressions.
newtype Eval a = Eval { runEval :: IO (Either Error a) }
  deriving
    ( Functor, Applicative, Monad
    , MonadError Error
    , MonadIO, MonadThrow, MonadCatch, MonadMask
    )
    via ExceptT Error IO
