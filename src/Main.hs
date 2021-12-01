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
import Control.Monad.Reader (ask, local)
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

import Builtins (loadPrelude)
import Core (evalFile, progn)
import Parser (pExprs)
import Types (Error(..), Eval(..), Bubble(..), Expr(..), runProgram)

tryError :: MonadError e m => m a -> m (Either e a)
tryError act = fmap Right act `catchError` (pure . Left)

handleBubble :: MonadIO m => (a -> m ()) -> Either Bubble a -> m ()
handleBubble h = \case
  Left (EvalError (Error e)) -> liftIO $ putStrLn $ Text.unpack e
  Right v -> h v

handleExceptions :: forall m. (MonadIO m, MonadCatch m) => m () -> m ()
handleExceptions = flip catches
  [ Handler $ \(e :: ExitCode) -> throw e
  , Handler $ \(e :: SomeException) -> liftIO $ liftIO $ putStrLn $ "<toplevel>: exception: " <> displayException e
  ]

-- | Load the prelude and execute a program
loadAndRun :: Eval a -> IO (Either Bubble a)
loadAndRun act = runProgram (loadPrelude >>= \env' -> local (const env') act)

repl :: IO ()
repl = do
  res <- loadAndRun $ runInputT defaultSettings $ loop Nothing
  handleBubble (\_ -> pure ()) res
  where
    run :: [Expr] -> InputT Eval ()
    run exprs = lift $ handleExceptions $ do
      env <- ask
      res <- tryError (progn env exprs)
      handleBubble (\case LInert -> pure (); e -> liftIO (print e)) res
    loop :: Maybe Text -> InputT Eval ()
    loop pending = do
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
              loop Nothing
            Done _ es -> run es *> loop Nothing
            Partial f ->
              case f "" of
                Done _ es -> run es *> loop Nothing
                _ -> loop (Just (fromMaybe "" pending <> line <> "\n"))

runFile :: String -> IO ()
runFile path = do
  contents <- Text.IO.readFile path
  handleExceptions $ do
    res <- loadAndRun $ do env <- ask; evalFile env contents
    handleBubble (\_ -> pure ()) res

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [path] -> runFile path
  _ -> putStrLn "error: too many arguments"
