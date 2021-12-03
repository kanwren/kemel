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
import Data.Functor (($>), (<&>))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split qualified as Split
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

pId :: Parser Symbol
pId = label "keyword" $ Symbol <$> choice
  [ (<>) <$> (mk . Text.singleton <$> idHeadChar) <*> (mk . Text.pack <$> many idChar)
  , mk <$> choice [string "+", string "-"]
  ]
  where
    idHeadChar = letter <|> satisfy (\c -> Text.any (c ==) "+-*/!$%&:<=>?@^_~")
    idChar = idHeadChar <|> digit <|> satisfy (\c -> Text.any (c ==) "+-#.")

between :: Parser a -> Parser b -> Parser c -> Parser c
between s e m = s *> m <* e

quote :: Expr -> Expr
quote v = LList [LSymbol "$quote", v]

pAtom :: Parser Expr
pAtom = choice
  [ label "string literal" $ LString . Text.pack <$> (char '"' *> ((char '\\' *> anyChar) <|> anyChar) `manyTill` char '"')
  , label "keyword" $ char ':' *> (LKeyword . Keyword <$> pId)
  , label "inert literal" $ string "#inert" $> LInert
  , label "ignore literal" $ string "#ignore" $> LIgnore
  , label "bool literal" $ fmap LBool $ (string "#f" $> False) <|> (string "#t" $> True)
  , label "number literal" $ LInt <$> signed decimal
  , label "symbol" $ LSymbol <$> pId
  ]

pExpr :: Parser Expr
pExpr = choice [pList, pAtom, symbol "`" *> pBackquoteExpr]
  where
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

data Splice = ExprSplice | ListSplice

-- An xpression in a backquote
pBackquoteExpr :: Parser Expr
pBackquoteExpr = do
  optional pSplice >>= \case
    Nothing -> pPendingSpliceExpr
    Just ExprSplice -> pExpr
    Just ListSplice -> fail "list splice after backquote is not allowed"
  where
    pSplice :: Parser Splice
    pSplice = (symbol ",@" $> ListSplice) <|> (symbol "," $> ExprSplice)

    pPendingSpliceExpr :: Parser Expr
    pPendingSpliceExpr = choice [pQuoteBackquoteed, pListBackquoteed, quote <$> pAtom, quote <$> (symbol "`" *> pBackquoteExpr)]
      where
        pQuoteBackquoteed = label "quoted backquote-expression" $ symbol "'" *> do
          optional pSplice >>= \case
            Nothing -> quote <$> pPendingSpliceExpr
            Just ExprSplice -> pExpr <&> \e -> LList [LSymbol "list", LList [LSymbol "$quote", LSymbol "$quote"], e]
            Just ListSplice -> pExpr <&> \e -> LList [LSymbol "cons", LList [LSymbol "$quote", LSymbol "$quote"], e]
        pListBackquoteed = label "quoted backquote-list" $ do
          let expr' = optional pSplice >>= \sp -> (sp,) <$> case sp of
                Nothing -> pPendingSpliceExpr
                Just _  -> pExpr
          between (symbol "(") (char ')') $ do
            leading <- expr' `sepEndBy` space
            case NonEmpty.nonEmpty leading of
              Nothing -> pure $ LList [LSymbol "$quote", LList []]
              Just neLeading -> optional (symbol "." *> lexeme expr') >>= \case
                Nothing -> pure $ spliceBackquotedList neLeading
                Just (Just ListSplice, _) -> fail "list splices after a dot are not allowed"
                Just (_, x) -> pure $ LList [LSymbol "append", spliceBackquotedList neLeading, x]

spliceBackquotedList :: NonEmpty (Maybe Splice, Expr) -> Expr
spliceBackquotedList (NonEmpty.toList -> xs)
  | any (\case (Just ListSplice, _) -> True; _ -> False) xs =
    let
      listSplice :: [(Maybe Splice, Expr)] -> [Expr]
      listSplice ys =
        let
          addSplices :: [Expr] -> [(Maybe Splice, Expr)] -> [Expr]
          addSplices acc [] = acc
          addSplices acc ((Just ListSplice, s):ss) = acc ++ s:fmap snd ss
          addSplices acc ((_, s):ss) = acc ++ [LList (LSymbol "list" : s:fmap snd ss)]
        in foldl' addSplices [] $ Split.split (Split.whenElt (\case (Just ListSplice, _) -> True; _ -> False)) ys
    in LList $ LSymbol "append":listSplice xs
  | otherwise = LList $ LSymbol "list":fmap snd xs
  -- TODO: optimizations:
  -- - (x y z ... ,v) could be (list* 'x 'y 'z ... v)
  -- - (,v ... x y z) could be (cons v (... 'x 'y 'z))
  -- - (,@x) could be x, but is (append x)
  -- - if there are no splices, the whole list could be quoted, but due to the
  --   preemptive quoting in pPendingSpliceExpr, they'll already be quoted

pExprs :: Parser [Expr]
pExprs = space *> manyTill (lexeme pExpr) endOfInput

parseFile :: Text -> Either String [Expr]
parseFile = parseOnly pExprs
