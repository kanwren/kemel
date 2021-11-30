{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (SomeException, displayException, throw)
import Control.Monad.Catch (catches, Handler(..), MonadCatch)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Text qualified as Text
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)
import System.Environment (getArgs)
import System.Exit (ExitCode)
import TextShow (showt)

import Builtins (mkBuiltins)
import Core (eval, evalFile)
import Parser (parseLine)
import Types (Error(..), Eval(..), Bubble(..), renderTagName, Expr(LList))
import Data.Default (def)

tryError :: MonadError e m => m a -> m (Either e a)
tryError act = fmap Right act `catchError` (pure . Left)

handleBubble :: MonadIO m => (a -> m ()) -> Either Bubble a -> m ()
handleBubble h = \case
  Left (EvalError (Error e)) -> liftIO $ putStrLn $ Text.unpack e
  Left (ReturnFrom blockName _) -> liftIO $ putStrLn $ Text.unpack $
    "<toplevel>: error returning from block " <> showt (showt blockName) <> ": no such block in scope"
  Left (TagGo tagName) -> liftIO $ putStrLn $ Text.unpack $
    "<toplevel>: error going to tag " <> renderTagName tagName <> ": no such tag in scope"
  Right v -> h v

handleExceptions :: forall m. (MonadIO m, MonadCatch m) => m () -> m ()
handleExceptions = flip catches
  [ Handler $ \(e :: ExitCode) -> throw e
  , Handler $ \(e :: SomeException) -> liftIO $ liftIO $ putStrLn $ "<toplevel>: exception: " <> displayException e
  ]

repl :: IO ()
repl = do
  builtins <- mkBuiltins
  (res, _) <- runEval (runInputT defaultSettings loop) builtins def
  handleBubble (\_ -> pure ()) res
  where
    loop :: InputT Eval ()
    loop = getInputLine "> " >>= \case
      Nothing -> pure ()
      Just line -> do
        -- TODO: resume!
        case parseLine line of
          Left e -> liftIO $ putStrLn e
          Right Nothing -> pure () -- comment line or empty line
          Right (Just expr) -> lift $ handleExceptions $ do
            env <- ask
            res <- tryError (eval env expr)
            handleBubble (\case LList [] -> pure (); e -> liftIO (print e)) res
        loop

runFile :: String -> IO ()
runFile path = do
  contents <- readFile path
  builtins <- mkBuiltins
  handleExceptions $ do
    (res, _) <- runEval (ask >>= \env -> evalFile env contents) builtins def
    handleBubble (\_ -> pure ()) res

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [path] -> runFile path
  _ -> putStrLn "error: too many arguments"
