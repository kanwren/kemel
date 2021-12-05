{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Exception (Exception)
import Control.Monad.Catch (MonadMask(..), MonadCatch(..), MonadThrow, ExitCase (ExitCaseSuccess))
import Control.Monad.Cont (ContT(..), MonadCont)
import Control.Monad.Except (ExceptT(ExceptT), MonadError(..))
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
data Closure r = Closure
  { closureParams :: ParamTree
  , closureDynamicEnv :: Binder
  , closureStaticEnv :: Environment r
  , closureBody :: Expr r
  }

type Builtin r = Environment r -> [Expr r] -> Eval r (Expr r)

-- | A callable operative, which is either a builtin defined from Haskell (a
-- plain Haskell function) or a user-defined vau closure.
data Operative r = BuiltinOp (Builtin r) | UserOp !(Closure r)

-- | A combiner at the head of a call is either _operative_ or _applicative_;
-- applicatives will first evaluate their arguments before calling the
-- underlying combiner.
data Combiner r = OperativeCombiner (Operative r) | ApplicativeCombiner (Combiner r)

instance TextShow (Combiner r) where
  showb = \case
    OperativeCombiner{} -> "<operative>"
    ApplicativeCombiner{} -> "<applicative>"

data Encapsulation r = Encapsulation !Unique !(Expr r)
  deriving stock Eq

data Expr r
  = LInert
  | LIgnore
  | LInt !Integer
  | LBool !Bool
  | LKeyword !Keyword
  | LString !Text
  | LSymbol !Symbol
  | LEncapsulation !(Encapsulation r)
  | LEnv !(Environment r)
  | LContinuation (Expr r -> Eval r (Expr r))
  | LList ![Expr r]
  | LDottedList !(NonEmpty (Expr r)) !(Expr r)
  | LCombiner !(Combiner r)
  deriving Show via (TextShow.FromTextShow (Expr r))

instance Eq (Expr r) where
  LInert == LInert = True
  LIgnore == LIgnore = True
  LInt x == LInt y = x == y
  LBool x == LBool y = x == y
  LKeyword x == LKeyword y = x == y
  LString x == LString y = x == y
  LSymbol x == LSymbol y = x == y
  LEncapsulation e1 == LEncapsulation e2 = e1 == e2
  LEnv x == LEnv y = x == y
  LContinuation _ == LContinuation _ = False
  LList x == LList y = x == y
  LDottedList x x' == LDottedList y y' = x == y && x' == y'
  LCombiner (ApplicativeCombiner c1) == LCombiner (ApplicativeCombiner c2) = (==) (LCombiner c1) (LCombiner c2)
  LCombiner (OperativeCombiner _) == LCombiner (OperativeCombiner _) = False -- TODO
  _ == _ = False

renderType :: Expr r -> Text
renderType = showt . typeToSymbol

typeToSymbol :: Expr r -> Symbol
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
  LContinuation _ -> "continuation"
  LList [] -> "null"
  LList (_:_) -> "pair"
  LDottedList _ _ -> "pair"
  LCombiner (OperativeCombiner _) -> "operative"
  LCombiner (ApplicativeCombiner _) -> "applicative"

symbolToTypePred :: Symbol -> Maybe (Expr r -> Bool)
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
  "continuation" -> pure $ \case LContinuation _ -> True; _ -> False
  "null" -> pure $ \case LList [] -> True; _ -> False
  "list" -> pure $ \case LList _ -> True; LDottedList _ _ -> True; _ -> False
  "pair" -> pure $ \case LDottedList _ _ -> True; LList (_:_) -> True; _ -> False
  "combiner" -> pure $ \case LCombiner _ -> True; _ -> False
  "operative" -> pure $ \case LCombiner (OperativeCombiner _) -> True; _ -> False
  "applicative" -> pure $ \case LCombiner (ApplicativeCombiner _) -> True; _ -> False
  _ -> Nothing

instance TextShow (Expr r) where
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
    LContinuation _ -> "<continuation>"
    LList [] -> "()"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb (NonEmpty.toList xs)) <> " . " <> showb x <> ")"
    LCombiner c -> showb c

newtype Error = EvalError Text

-- | A handle to a mapping of variable names to values, along with any parent
-- environments.
data Environment r =
  Environment
    (HIO.BasicHashTable Symbol (Expr r))
    -- ^ The variable bindings in this environment
    [Environment r]
    -- ^ The parent environments

instance Eq (Environment r) where
  _ == _ = False -- TODO: equate on STRefs

newEnvironment :: [Environment r] -> IO (Environment r)
newEnvironment parents = do
  m <- HIO.new
  pure $ Environment m parents

newEnvironmentWith :: [(Symbol, Expr r)] -> [Environment r] -> IO (Environment r)
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
    in (new, cur)
  pure $ Symbol $ mk $ "#:g" <> showt n

-- | The monad for evaluating expressions.
newtype Eval r a = Eval { runEval :: (a -> IO (Either Error r)) -> IO (Either Error r) }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow
    , MonadCont
    )
    via ContT r (ExceptT Error IO)

instance MonadError Error (Eval r) where
  throwError :: Error -> Eval r a
  throwError e = Eval $ \_ -> pure (Left e)

  catchError :: Eval r a -> (Error -> Eval r a) -> Eval r a
  catchError act handler = Eval $ \k -> do
    runEval act k >>= \case
      Right x -> pure $ Right x
      Left e -> runEval (handler e) k

-- TODO: make this valid
instance MonadCatch (Eval r) where
  catch :: forall e a. Exception e => Eval r a -> (e -> Eval r a) -> Eval r a
  catch (Eval k) handler = Eval $ \h ->
    k h `catch` \(err :: e) -> runEval (handler err) h

-- TODO: make this not a hack
instance MonadMask (Eval r) where
  mask :: ((forall a. Eval r a -> Eval r a) -> Eval r b) -> Eval r b
  mask f = f id

  uninterruptibleMask :: ((forall a. Eval r a -> Eval r a) -> Eval r b) -> Eval r b
  uninterruptibleMask f = f id

  generalBracket :: Eval r a -> (a -> ExitCase b -> Eval r c) -> (a -> Eval r b) -> Eval r (b, c)
  generalBracket x r k = do
    x' <- x
    b <- k x'
    c <- r x' (ExitCaseSuccess b)
    pure (b, c)

-- ContT Expr (ExceptT Error IO) a
-- (a -> m r) -> m r
-- (a -> ExceptT Error IO Expr) -> ExceptT Error IO Expr
-- (a -> IO (Either Error Expr)) -> IO (Either Error Expr)
