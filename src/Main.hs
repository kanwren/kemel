{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)
import System.Environment (getArgs)

import Builtins (makeGround)
import Core (evalFile, progn)
import Parser (pExprs)
import Types (Eval(..), Expr(..), Environment, Error(..))

handleError :: MonadIO m => (a -> m ()) -> Either Error a -> m ()
handleError h = \case
  Left (EvalError e) -> liftIO $ putStrLn $ Text.unpack e
  Right v -> h v

handleExceptions :: (MonadIO m, MonadCatch m) => m () -> m ()
handleExceptions = flip catch $ \(e :: SomeException) -> liftIO $ putStrLn $ "<toplevel>: exception: " <> displayException e

-- | Run a program in a child environment of the standard environment
loadAndRun :: (Environment -> Eval a) -> IO (Either Error a)
loadAndRun act = runEval $ makeGround >>= act

repl :: IO ()
repl = do
  res <- loadAndRun $ \env -> runInputT defaultSettings (loop env Nothing)
  handleError (\_ -> pure ()) res
  where
    runLine :: Environment -> [Expr] -> InputT Eval ()
    runLine env exprs = lift $ handleExceptions $ do
      res <- (Right <$> progn env exprs) `catchError` (pure . Left)
      handleError (\case LInert -> pure (); e -> liftIO (print e)) res
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
            Done _ es -> runLine env es *> loop env Nothing
            Partial f ->
              case f "" of
                Done _ es -> runLine env es *> loop env Nothing
                _ -> loop env (Just (fromMaybe "" pending <> line <> "\n"))

runFile :: String -> IO ()
runFile path = do
  contents <- Text.IO.readFile path
  handleExceptions $ do
    res <- loadAndRun $ \env -> evalFile env contents
    handleError (\_ -> pure ()) res

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [path] -> runFile path
  _ -> putStrLn "error: too many arguments"
