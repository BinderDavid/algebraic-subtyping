module Parser
  ( parseTerm
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import Syntax

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

termParser :: Parser Term
termParser = choice
  [ try appParser
  , try selParser
  , parenP
  , lamParser
  , litParser
  , varParser
  , rcdParser
  ]

-- | Does not try to parse an application or selection.
termParserNR :: Parser Term
termParserNR = choice
  [ parenP
  , lamParser
  , litParser
  , varParser
  , rcdParser
  ]


parenP :: Parser Term
parenP = do
  _ <- try $ symbol "("
  t <- termParser
  _ <- symbol ")"
  return t

litParser :: Parser Term
litParser = TmLit <$> lexeme L.decimal

varParser :: Parser Term
varParser = TmVar <$>  lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

lamParser :: Parser Term
lamParser = do
  _ <- try $ symbol "\\"
  v <- lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  _ <- symbol "."
  t <- termParser
  return (TmLam v t)

appParser :: Parser Term
appParser = do
  tm1 <- termParserNR
  tm2 <- termParser
  return (TmApp tm1 tm2)

rcdParser :: Parser Term
rcdParser = do
  _ <- symbol "{"
  cnts <- flip sepBy (symbol ",") $ do
    lbl <- lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "label")
    _ <- symbol "="
    tm <- termParser
    return (lbl, tm)
  _ <- symbol "}"
  return (TmRcd cnts)

selParser :: Parser Term
selParser = do
  tm <- termParserNR
  _ <- symbol "."
  lbl <- lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "label")
  return (TmSel tm lbl)

parseTerm :: String -> Either String Term
parseTerm input = case runParser (sc >> termParser) "<interactive>" input of
  Left err -> Left (errorBundlePretty err)
  Right res -> Right res
