module Parser where

import Debug.Trace

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

----------------------------------------------------------------
-- Parse Term

unit :: Parser Term
unit = do
  reserved "unit"
  return TmUnit

true :: Parser Term
true = do
  reserved "true"
  return TmTrue

false :: Parser Term
false = do
  reserved "false"
  return TmFalse

parseIf :: Parser Term
parseIf = do
  reserved "if"
  t1 <- parseTerm
  reserved "then"
  t2 <- parseTerm
  reserved "else"
  t3 <- parseTerm
  return $ TmIf t1 t2 t3

parseProd :: Parser Term
parseProd = parens $ do
  t1 <- parseTerm
  comma
  t2 <- parseTerm
  return $ TmProd t1 t2

parseProj :: Parser (Term -> Term)
parseProj = do
  dot
  num <- many1 digit
  return $ \t -> if num == "1" then TmFst t else TmSnd t
-- TODO: how to throw a ParseError?

parsePrimTerm :: Parser Term
parsePrimTerm 
  =  unit
 <|> trace "<processing true>" true
 <|> trace "<processing false>" false
 <|> parseIf
 <|> parseProd

termOps = [ [Ex.Postfix parseProj ] ]

parseTermExpr :: Parser Term
parseTermExpr = Ex.buildExpressionParser termOps parsePrimTerm

parseTerm :: Parser Term
parseTerm = parseTermExpr

runMyParser :: String -> Either ParseError Term
runMyParser = parse parseTerm "<CandyQwQ>"

----------------------------------------------------------------
-- Parse Type
binary s f assoc = Ex.Infix (reservedOp s >> return f) assoc

table = [ [binary "*" TyProd Ex.AssocLeft]
        , [binary "->" TyArr Ex.AssocRight]
        ]
        -- ,[binary "~>" TyCir Ex.AssocLeft,
          -- binary "-" Minus Ex.AssocLeft]]

tyExpr :: Parser Type
tyExpr = Ex.buildExpressionParser table parseType

parseType :: Parser Type
parseType = error "not implemented yet"