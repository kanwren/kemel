{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Parser (pExprs, parseFile) where

import Control.Applicative ((<|>), many)
import Control.Applicative.Combinators (sepEndBy, optional)
import Control.Monad (void)
import Data.Attoparsec.Text as AT hiding (space)
import Data.CaseInsensitive (mk)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text

import Types

space :: Parser ()
space = skipMany $ choice [void (takeWhile1 isSpace), void (string ";" *> AT.manyTill anyChar (endOfLine <|> endOfInput))]

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: Text -> Parser Text
symbol s = string s <* space

label :: String -> Parser a -> Parser a
label l f = f <?> l

pExpr :: Parser Expr
pExpr = choice
  [ pList
  , label "string literal" $ LString . Text.pack <$> (char '"' *> ((char '\\' *> anyChar) <|> anyChar) `manyTill` char '"')
  , label "keyword" $ char ':' *> (LKeyword . Keyword <$> pId)
  , label "inert literal" $ string "#inert" $> LInert
  , label "ignore literal" $ string "#ignore" $> LIgnore
  , label "bool literal" $ fmap LBool $ (string "#f" $> False) <|> (string "#t" $> True)
  , label "number literal" $ LInt <$> signed decimal
  , label "symbol" $ LSymbol <$> pId
  ]
  where
    pId = label "keyword" $ Symbol <$> choice
      [ (<>) <$> (mk . Text.singleton <$> idHeadChar) <*> (mk . Text.pack <$> many idChar)
      , mk <$> choice [string "+", string "-"]
      ]
      where
        idHeadChar = letter <|> satisfy (\c -> Text.any (c ==) "+-*/!$%&:<=>?@^_~")
        idChar = idHeadChar <|> digit <|> satisfy (\c -> Text.any (c ==) "+-#.")
    between s e m = s *> m <* e
    pList = label "list" $ between (symbol "(") (char ')') $ do
      leading <- pExpr `sepEndBy` space
      case NonEmpty.nonEmpty leading of
        Nothing -> pure $ LList leading
        Just neLeading -> optional (symbol "." *> lexeme pExpr) >>= \case
          Nothing -> pure $ LList leading
          Just (LList xs) -> pure $ LList (leading ++ xs)
          Just (LDottedList xs x) -> pure $ LDottedList (prependList leading xs) x
          Just end -> pure $ LDottedList neLeading end
    prependList :: [a] -> NonEmpty a -> NonEmpty a
    prependList [] (y:|ys) = y:|ys
    prependList (x:xs) (y:|ys) = x :| (xs ++ y : ys)

pExprs :: Parser [Expr]
pExprs = space *> manyTill (lexeme pExpr) endOfInput

parseFile :: Text -> Either String [Expr]
parseFile = parseOnly pExprs
