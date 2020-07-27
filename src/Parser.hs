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
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

termParser :: Parser Term
termParser = litParser <|>
             varParser <|>
             lamParser <|>
             appParser -- <|>
             -- rcdParser <|>
             -- selParser

litParser :: Parser Term
litParser = TmLit <$> lexeme L.decimal

varParser :: Parser Term
varParser = TmVar <$>  lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

lamParser :: Parser Term
lamParser = do
  _ <- symbol "\\"
  v <- ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  _ <- symbol "."
  t <- termParser
  return (TmLam v t)

appParser :: Parser Term
appParser = do
  _ <- symbol "("
  tm1 <- termParser
  tm2 <- termParser
  _ <- symbol ")"
  return (TmApp tm1 tm2)

rcdParser :: Parser Term
rcdParser = return (TmVar "RCDPARSER")

selParser :: Parser Term
selParser = return (TmVar "SELPARSER")

parseTerm :: String -> Either String Term
parseTerm input = case runParser termParser "<interactive>" input of
  Left err -> Left (errorBundlePretty err)
  Right res -> Right res
