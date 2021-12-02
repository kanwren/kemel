{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, displayException, throw)
import Control.Monad.Catch (catches, Handler(..), MonadCatch)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.Attoparsec.Text (parse, IResult(..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)
import System.Environment (getArgs)
import System.Exit (ExitCode)

import Builtins (makeGround)
import Core (evalFile, progn)
import Parser (pExprs)
import Types (Eval(..), Bubble(..), Expr(..), runProgram, Environment)

tryError :: MonadError e m => m a -> m (Either e a)
tryError act = fmap Right act `catchError` (pure . Left)

handleBubble :: MonadIO m => (a -> m ()) -> Either Bubble a -> m ()
handleBubble h = \case
  Left (EvalError e) -> liftIO $ putStrLn $ Text.unpack e
  Right v -> h v

handleExceptions :: (MonadIO m, MonadCatch m) => m () -> m ()
handleExceptions = flip catches
  [ Handler $ \(e :: ExitCode) -> throw e
  , Handler $ \(e :: SomeException) -> liftIO $ liftIO $ putStrLn $ "<toplevel>: exception: " <> displayException e
  ]

-- | Run a program in a child environment of the standard environment
loadAndRun :: (Environment -> Eval a) -> IO (Either Bubble a)
loadAndRun act = runProgram $ makeGround >>= act

repl :: IO ()
repl = do
  res <- loadAndRun $ \env -> runInputT defaultSettings $ loop env Nothing
  handleBubble (\_ -> pure ()) res
  where
    run :: Environment -> [Expr] -> InputT Eval ()
    run env exprs = lift $ handleExceptions $ do
      res <- tryError (progn env exprs)
      handleBubble (\case LInert -> pure (); e -> liftIO (print e)) res
    loop :: Environment -> Maybe Text -> InputT Eval ()
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
            Done _ es -> run env es *> loop env Nothing
            Partial f ->
              case f "" of
                Done _ es -> run env es *> loop env Nothing
                _ -> loop env (Just (fromMaybe "" pending <> line <> "\n"))

runFile :: String -> IO ()
runFile path = do
  contents <- Text.IO.readFile path
  handleExceptions $ do
    res <- loadAndRun $ \env -> evalFile env contents
    handleBubble (\_ -> pure ()) res

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [path] -> runFile path
  _ -> putStrLn "error: too many arguments"
