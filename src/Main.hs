{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative.Combinators (optional)
import Control.Exception (SomeException, displayException)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.Attoparsec.Text (parse, IResult(..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, Settings(..), setComplete)
import System.Console.Haskeline.Completion
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>))
import Data.HashTable.IO qualified as HIO
import TextShow (showt)
import Control.Monad.Cont (callCC)

import Builtins (makeGround)
import Core (evalFile, progn, defineVar)
import Parser (pExprs)
import Types (Eval(..), Expr(..), Environment(..), Error(..), Symbol)
import Control.Monad (void)

handleError :: MonadIO m => (a -> m ()) -> Either Error a -> m ()
handleError h = \case
  Left (EvalError e) -> liftIO $ putStrLn $ Text.unpack e
  Right v -> h v

handleExceptions :: (MonadIO m, MonadCatch m) => m () -> m ()
handleExceptions = flip catch $ \(e :: SomeException) -> liftIO $ putStrLn $ "<toplevel>: exception: " <> displayException e

-- | Run a program in a child environment of the standard environment
loadAndRun :: (Environment () -> Eval () b) -> IO ()
loadAndRun act = void $ flip runEval pure $ do
  void $ callCC $ \k -> do
    _ <- act =<< makeGround k
    pure LInert
  pure $ Right ()

-- TODO: don't try to complete in a string or after a comment
completeSymbol :: Environment r -> CompletionFunc (Eval r)
completeSymbol env =
    completeWord Nothing " \t\n\r();\"" search
    `fallbackCompletion`
    completeFilename
  where
    search prefix = do
      let prefix' = Text.pack prefix
      symbols <- liftIO $ fmap showt <$> walkEnvironment env
      let valid = filter (Text.isPrefixOf prefix') symbols
      pure $ fmap (simpleCompletion. Text.unpack) valid
    walkEnvironment :: Environment r -> IO [Symbol]
    walkEnvironment (Environment table parents) = (++)
      <$> do fmap fst <$> HIO.toList table
      <*> do concat <$> traverse walkEnvironment parents

repl :: IO ()
repl = do
  loadAndRun $ \env -> do
    home <- liftIO $ optional (getEnv "HOME")
    let histFile = (</> ".kemel_history") <$> home
    let settings = setComplete (completeSymbol env) $ defaultSettings { historyFile = histFile, autoAddHistory = True }
    runInputT settings (loop env Nothing)
  where
    runLine :: Environment r -> [Expr r] -> InputT (Eval r) ()
    runLine env exprs = lift $ handleExceptions $ do
      let
        -- TODO: log errors (probably by adding an error type to the AST and
        -- removing ExceptT)
        code = callCC $ \k -> do
          defineVar env "error-continuation" (LContinuation k)
          progn env exprs
      res <- (Right <$> code) `catchError` (pure . Left)
      handleError (\case LInert -> pure (); e -> liftIO (print e)) res
    loop :: Environment r -> Maybe Text -> InputT (Eval r) ()
    loop env pending = do
      input <- getInputLine $ case pending of Nothing -> "> "; Just _ -> "...| "
      case Text.pack <$> input of
        Nothing -> pure ()
        Just line -> do
          case parse pExprs (fromMaybe "" pending <> line) of
            Fail rest ctx msg -> do
              liftIO $ do
                putStrLn $ "<toplevel>: parse error at " <> show (takeWhile (/= '\n') (take 20 (Text.unpack rest)))
                putStrLn "context: "
                traverse_ (putStrLn . ("  " ++)) ctx
                putStrLn msg
              loop env Nothing
            Done _ es -> runLine env es *> loop env Nothing
            Partial f ->
              case f "" of
                Done _ es -> runLine env es *> loop env Nothing
                _ -> loop env (Just (fromMaybe "" pending <> line <> "\n"))

runFile :: String -> IO ()
runFile path = do
  contents <- Text.IO.readFile path
  handleExceptions $ loadAndRun $ \env -> do
    callCC $ \k -> do
      defineVar env "error-continuation" (LContinuation k)
      evalFile env contents

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [path] -> runFile path
  _ -> putStrLn "error: too many arguments"
