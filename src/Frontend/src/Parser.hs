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
  return $ \t -> case num of
    "1" -> TmFst t
    "2" -> TmSnd t
    _ -> error "Product Index Error"

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
  ty <- parseType
  dot
  ctx <- getState
  setState $ addBinding ctx (var, VarBind ty)
  term <- parseTerm
  setState ctx
  return $ TmAbs var ty term

parseRun :: Parser Term
parseRun = do
  reserved "run"
  circ <- parseCirc
  return $ TmRun circ

parseCAbs :: Parser Term
parseCAbs = do
  reservedOp "/"
  pat <- parsePattern
  colon
  wtype <- parseWtype
  dot
  ctx <- getState
  -- TODO: need to travel through all wire variables in the pat
  -- setState $ addBinding ctx (Wir pat, WireBind wtype)
  circ <- parseCirc
  setState ctx
  return $ TmCir pat wtype circ

parsePrimTerm :: Parser Term -- without projection
parsePrimTerm = whiteSpace >> (
      unit
  <|> true
  <|> false
  <|> parseIf
  <|> parseVar
  <|> parseAbs
  <|> try parseProd
  <|> parens parseTerm
  )

termOps :: [[Ex.Operator String Context Identity Term]]
termOps = [ [Ex.Postfix parseProj] ]

parseTermExpr :: Parser Term -- deal with projection
parseTermExpr = whiteSpace >> Ex.buildExpressionParser termOps parsePrimTerm

parseTerm :: Parser Term -- all terms
parseTerm = whiteSpace >> chainl1 parseTermExpr (return TmApp)

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
parsePrimType = whiteSpace >> (
      parseTyUnit
  <|> parseTyBool
  <|> parens parseType
  )

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
parseType = whiteSpace >> parseTypeExpr

----------------------------------------------------------------
-- Parse Circuit
parseOutput :: Parser Circ
parseOutput = do
  reserved "output"
  pat <- parsePattern
  return $ CcOutput pat

parseGateApp :: Parser Circ
parseGateApp = do
  pat2 <- parsePattern
  reservedOp "<-"
  reserved "gate"
  gate <- parseGate
  pat1 <- parsePattern
  semi
  circ <- parseCirc
  return $ CcGate pat2 gate pat1 circ

parseComp :: Parser Circ
parseComp = do
  pat <- parsePattern
  reservedOp "<-"
  circ1 <- parseCirc
  semi
  circ2 <- parseCirc
  return $ CcComp pat circ1 circ2

-- TODO: not finished

parseCirc :: Parser Circ
parseCirc = whiteSpace >> (
      parseOutput
  <|> parseGateApp
  <|> parseComp
  <|> parens parseCirc
  )

----------------------------------------------------------------
-- Parse Circuit Type
parseOne :: Parser Wtype
parseOne = reservedOp "One" >> return WtUnit

parseBit :: Parser Wtype
parseBit = reservedOp "Bit" >> return WtBit

parseQubit :: Parser Wtype
parseQubit = reservedOp "Qubit" >> return WtQubit

parseWProd :: Parser Wtype
parseWProd = do
  w1 <- parseWtype
  reservedOp "#"
  w2 <- parseWtype
  return $ WtProd w1 w2

parseWtype :: Parser Wtype
parseWtype = whiteSpace >> (
      parseOne
  <|> parseBit
  <|> parseQubit
  <|> parseWProd
  <|> parens parseWtype
  )

----------------------------------------------------------------
-- Parse Pattern

parseWVar :: Parser Pattern
parseWVar = do
  var <- identifier
  ctx <- getState
  idx <- name2index ctx var
  return $ PtVar idx (length ctx)

parsePattern :: Parser Pattern
parsePattern = error "No"

----------------------------------------------------------------
-- Parse Pattern
parseGate :: Parser Gate
parseGate = error "No"

----------------------------------------------------------------
runMyParser :: String -> Either ParseError Term
runMyParser = runParser parseTerm emptyctx "<CandyQwQ>" 
