{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Parser (parseLine, parseFile) where

import Control.Arrow (left)
import Control.Monad (guard)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split qualified as Split
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

import Types
import Data.List (foldl')

type Parser = M.Parsec Void String

space :: Parser ()
space = MCL.space MC.space1 (MCL.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme space

symbol :: String -> Parser String
symbol = MCL.symbol space

pNumber :: Parser Expr
pNumber = label "number literal" $ do
  num <- MCL.signed (pure ()) MCL.decimal
  pure $ LInt num

pBool :: Parser Expr
pBool = label "bool literal" $ fmap LBool $ (MC.string "#f" $> False) <|> (MC.string "#t" $> True)

pKeyword :: Parser Expr
pKeyword = label "keyword" $ MC.char ':' *> (LKeyword . Keyword <$> pId)

pString :: Parser Expr
pString = label "string literal" $ LString . Text.pack <$> (MC.char '"' *> M.manyTill MCL.charLiteral (MC.char '"'))

-- TODO: allow identifiers beginning with numbers to be arbitrary symbols
pId :: Parser Symbol
pId = label "keyword" $ do
    res <- fmap collect identifier
    guard $ res /= SimpleSymbol "."
    pure res
  where
    idHeadChar, idChar :: Parser Char
    idHeadChar = label "identifier first char" $ do
      MC.letterChar <|> M.oneOf ("+-*/!$%&:<=>?@^_~." :: String) <|> MC.numberChar
    idChar = label "identifier char" $ do
      idHeadChar <|> M.oneOf ("#" :: String)

    arbString :: Parser Text
    arbString = MC.char '|' *> (Text.pack <$> M.manyTill ((MC.string "\\|" $> '|') <|> M.anySingle) (MC.char '|'))

    firstChar :: Parser (Either Text (CI Text))
    firstChar = M.choice
      [ Left <$> arbString
      , Right . mk . Text.singleton <$> idHeadChar
      ]

    restChars :: Parser [Either Text (CI Text)]
    restChars = M.many $ M.choice
      [ Left <$> arbString
      , Right . mk . Text.pack <$> M.some idChar
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

pList :: Parser Expr
pList = label "list" $ between (symbol "(") (MC.char ')') $ do
  leading <- M.sepEndBy (try pExpr) space
  case NonEmpty.nonEmpty leading of
    Nothing -> pure $ LList leading
    Just neLeading -> optional (symbol "." *> lexeme pExpr) >>= \case
      Nothing -> pure $ LList leading
      Just (LList xs) -> pure $ LList (leading ++ xs)
      Just end -> pure $ LDottedList neLeading end

pExpr :: Parser Expr
pExpr = M.choice
  [ pQuote
  , pList
  , pString
  , pKeyword
  , pBool
  -- try needed due to signs in numbers, '-5' is a number but '-a' is a symbol
  , try pNumber
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
    pPendingSpliceExpr = M.choice
      [ pQuoteBackquoteed
      , pListBackquoteed
      , quote <$> pString
      , quote <$> pKeyword
      , quote <$> pBool
      , quote <$> try pNumber
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
          between (symbol "(") (MC.char ')') $ do
            leading <- M.sepEndBy (try expr') space
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

parseLine :: String -> Either String (Maybe Expr)
parseLine = left errorBundlePretty . M.parse (space *> optional (lexeme pExpr) <* M.eof) "repl"

pFile :: Parser [Expr]
pFile = space *> M.many (lexeme pExpr) <* M.eof

parseFile :: String -> Either String [Expr]
parseFile = left errorBundlePretty . M.parse pFile "load"

