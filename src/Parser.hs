{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Parser (pExprs, parseFile) where

import Control.Applicative ((<|>), many, some)
import Control.Monad (void, guard)
import Data.Attoparsec.Text as AT hiding (space)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.Char (isSpace)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split qualified as Split
import Data.Text (Text)
import Data.Text qualified as Text
import Control.Applicative.Combinators (sepEndBy, optional)

import Types
import Data.List (foldl')

space1 :: Parser ()
space1 = void $ takeWhile1 isSpace

space :: Parser ()
space = skipMany $ choice [space1, void (string ";" *> AT.manyTill anyChar (endOfLine <|> endOfInput))]

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: Text -> Parser Text
symbol s = string s <* space

label :: String -> Parser a -> Parser a
label l f = f <?> l

pNumber :: Parser Expr
pNumber = label "number literal" $ LInt <$> signed decimal

pBool :: Parser Expr
pBool = label "bool literal" $ fmap LBool $ (string "#f" $> False) <|> (string "#t" $> True)

pKeyword :: Parser Expr
pKeyword = label "keyword" $ char ':' *> (LKeyword . Keyword <$> pId)

-- TODO: character escapes
pString :: Parser Expr
pString = label "string literal" $ LString . Text.pack <$> (char '"' *> ((char '\\' *> anyChar) <|> anyChar) `manyTill` char '"')

-- TODO: allow identifiers beginning with numbers to be arbitrary symbols
pId :: Parser Symbol
pId = label "keyword" $ do
    res <- fmap collect identifier
    guard $ res /= SimpleSymbol "."
    pure res
  where
    idHeadChar, idChar :: Parser Char
    idHeadChar = label "identifier first char" $ do
      letter <|> satisfy (\c -> Text.any (c ==) "-+*/!$%&:<=>?@^_~.") <|> digit
    idChar = label "identifier char" $ do
      idHeadChar <|> char '#'

    arbString :: Parser Text
    arbString = char '|' *> (Text.pack <$> ((string "\\|" $> '|') <|> anyChar) `manyTill` char '|')

    firstChar :: Parser (Either Text (CI Text))
    firstChar = choice
      [ Left <$> arbString
      , Right . mk . Text.singleton <$> idHeadChar
      ]

    restChars :: Parser [Either Text (CI Text)]
    restChars = many $ choice
      [ Left <$> arbString
      , Right . mk . Text.pack <$> some idChar
      ]

    identifier :: Parser (NonEmpty (Either Text (CI Text)))
    identifier = (:|) <$> firstChar <*> restChars

    toArbSymbol :: NonEmpty (Either Text (CI Text)) -> Symbol
    toArbSymbol = ArbSymbol . Text.concat . fmap (either id foldedCase) . NonEmpty.toList

    collect :: NonEmpty (Either Text (CI Text)) -> Symbol
    collect chunks =
      case NonEmpty.filter (/= Left "") chunks of
        -- implies that entire identifier was ||
        []         -> ArbSymbol ""
        Right x:xs -> go x xs
        x:xs       -> toArbSymbol $ x:|xs
      where
        go :: CI Text -> [Either Text (CI Text)] -> Symbol
        go acc []           = SimpleSymbol acc
        go acc (Right x:xs) = go (acc <> x) xs
        go acc (Left x:xs)  = toArbSymbol $ Right acc:|(Left x:xs)

pSymbol :: Parser Expr
pSymbol = LSymbol <$> pId

-- 'x is the same as (quote x), where "quote" is a special form that returns
-- its only argument without evaluating it. This is coupled with the
-- corresponding builtin in the `Eval` module.
pQuote :: Parser Expr
pQuote = label "quoted expression" $ symbol "'" *> do
  e <- pExpr
  pure $ LList [LSymbol "quote", e]

between :: Parser a -> Parser b -> Parser c -> Parser c
between s e m = s *> m <* e

pList :: Parser Expr
pList = label "list" $ between (symbol "(") (char ')') $ do
  leading <- pExpr `sepEndBy` space
  case NonEmpty.nonEmpty leading of
    Nothing -> pure $ LList leading
    Just neLeading -> optional (symbol "." *> lexeme pExpr) >>= \case
      Nothing -> pure $ LList leading
      Just (LList xs) -> pure $ LList (leading ++ xs)
      Just end -> pure $ LDottedList neLeading end

pExpr :: Parser Expr
pExpr = choice
  [ pQuote
  , pList
  , pString
  , pKeyword
  , pBool
  , pNumber
  , pSymbol
  , symbol "`" *> pBackquoteExpr
  ]

data Splice = ExprSplice | ListSplice

pSplice :: Parser Splice
pSplice = (symbol ",@" $> ListSplice) <|> (symbol "," $> ExprSplice)

quote :: Expr -> Expr
quote v = LList [LSymbol "quote", v]

-- An xpression in a backquote
pBackquoteExpr :: Parser Expr
pBackquoteExpr = do
  optional pSplice >>= \case
    Nothing -> pPendingSpliceExpr
    Just ExprSplice -> pExpr
    Just ListSplice -> fail "list splice after backquote is not allowed"
  where
    pPendingSpliceExpr :: Parser Expr
    pPendingSpliceExpr = choice
      [ pQuoteBackquoteed
      , pListBackquoteed
      , quote <$> pString
      , quote <$> pKeyword
      , quote <$> pBool
      , quote <$> pNumber
      , quote <$> pSymbol
      , quote <$> (symbol "`" *> pBackquoteExpr)
      ]
      where
        pQuoteBackquoteed = label "quoted backquote-expression" $ symbol "'" *> do
          optional pSplice >>= \case
            Nothing -> quote <$> pPendingSpliceExpr
            Just ExprSplice -> pExpr <&> \e -> LList [LSymbol "list", LList [LSymbol "quote", LSymbol "quote"], e]
            Just ListSplice -> pExpr <&> \e -> LList [LSymbol "cons", LList [LSymbol "quote", LSymbol "quote"], e]
        pListBackquoteed = label "quoted backquote-list" $ do
          let expr' = optional pSplice >>= \sp -> (sp,) <$> case sp of
                Nothing -> pPendingSpliceExpr
                Just _  -> pExpr
          between (symbol "(") (char ')') $ do
            leading <- expr' `sepEndBy` space
            case NonEmpty.nonEmpty leading of
              -- NOTE: a notable difference between this and CLisp is that `()`
              -- will always just be NIL, regardless of depth of backquoting.
              -- For consistency, we keep the intuitive behavior here,
              -- especially since the other behavior means that for example:
              --     ````nil => NIL
              --     ````()  => NIL
              --     ''''nil => '''NIL
              --     ''''()  => '''NIL
              -- meaning that NIL and () have to be special-cased to be the
              -- same, regardless of the identifier
              Nothing -> pure $ LList [LSymbol "quote", LList []]
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
