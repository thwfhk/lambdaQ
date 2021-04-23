module Parser where

import Debug.Trace

import Text.Parsec
import Text.Parsec.Char (digit)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity (Identity)

import Lexer
import Syntax
import Context

type Parser a = ParsecT String Context Identity a
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
  whiteSpace -- interesting
  reserved "then"
  t2 <- parseTerm
  whiteSpace
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

parseVar :: Parser Term
parseVar = do
  var <- identifier
  ctx <- getState
  idx <- name2index ctx var
  return $ TmVar idx (length ctx)

parseAbs :: Parser Term
parseAbs = do
  reservedOp "\\"
  var <- identifier
  colon
  tyVar <- parseType
  dot
  ctx <- getState
  setState $ addBinding ctx (var, VarBind tyVar)
  term <- parseTerm
  setState ctx
  return $ TmAbs var tyVar term

parsePrimTerm :: Parser Term -- without projection
parsePrimTerm 
  =  unit
 <|> true
 <|> false
 <|> parseIf
 <|> parseVar
 <|> parseAbs
 <|> try parseProd
 <|> parens parseTerm

termOps :: [[Ex.Operator String Context Identity Term]]
termOps = [ [Ex.Postfix parseProj] ]

parseTermExpr :: Parser Term -- deal with projection
parseTermExpr = Ex.buildExpressionParser termOps parsePrimTerm

parseTerm :: Parser Term -- all terms
parseTerm = chainl1 parseTermExpr (return TmApp)

----------------------------------------------------------------
-- Parse Type

parseTyUnit :: Parser Type
parseTyUnit = do
  reserved "Unit"
  return TyUnit

parseTyBool :: Parser Type
parseTyBool = do
  reserved "Bool"
  return TyBool

-- parseTyProd :: Parser Type
-- parseTyProd = parens $ do
--   t1 <- parseType
--   comma
--   t2 <- parseType
--   return $ TyProd t1 t2

parsePrimType :: Parser Type
parsePrimType
   =  parseTyUnit
  <|> parseTyBool
  <|> parens parseType

binary :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String u Identity a
binary s f assoc = Ex.Infix (reservedOp s >> return f) assoc

typeOps :: [[Ex.Operator String u Identity Type]]
typeOps = [ [binary "*" TyProd Ex.AssocLeft]
          , [binary "->" TyArr Ex.AssocRight]
          ]
        -- ,[binary "~>" TyCir Ex.AssocLeft,
          -- binary "-" Minus Ex.AssocLeft]]

parseTypeExpr :: Parser Type
parseTypeExpr = Ex.buildExpressionParser typeOps parsePrimType

parseType :: Parser Type
parseType = parseTypeExpr

----------------------------------------------------------------
runMyParser :: String -> Either ParseError Term
runMyParser = runParser parseTerm emptyctx "<CandyQwQ>" 
