{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Parser (pExprs, parseFile) where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Attoparsec.Text as AT hiding (space)
import Data.CaseInsensitive (mk)
import Data.Char (isSpace)
import Data.Functor (($>))
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

pExpr :: Parser (Expr r)
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
    pList = label "list" $ symbol "(" *> pList'
    pList' = do
      let
        pNull = space *> char ')' $> LNull
        pPair = do
          car <- lexeme pExpr
          cdr <- pList' <|> (symbol "." *> lexeme pExpr <* char ')')
          pure $ LPair car cdr
      pNull <|> pPair

pExprs :: Parser [Expr r]
pExprs = space *> manyTill (lexeme pExpr) endOfInput

parseFile :: Text -> Either String [Expr r]
parseFile = parseOnly pExprs
