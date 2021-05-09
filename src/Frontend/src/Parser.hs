module Parser where

import Debug.Trace

import Text.Parsec
import Text.Parsec.Char (digit)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity (Identity)
import Control.Applicative (empty)

import Lexer
import Syntax
import Context

-- type Parser a = ParsecT String Context Identity a
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
  let idx = name2index ctx var
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
  patvar <- parsePattern True
  colon
  wtype <- parseWtype
  traceM $ "#####[WTYPE]: " ++ show wtype ++ " [END]#####"
  dot
  ctx <- getState
  addPatWtypeBindingP (patvar, wtype)
  circ <- parseCirc
  setState ctx
  return $ TmCir patvar wtype circ

parsePrimTerm :: Parser Term -- without projection
parsePrimTerm = (whiteSpace >>) $
      unit
  <|> true
  <|> false
  <|> parseIf
  <|> parseVar
  <|> parseAbs
  <|> try parseProd
  <|> parseRun
  <|> parseCAbs
  <|> parens parseTerm

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

parseTyCir :: Parser Type
parseTyCir = do
  t1 <- parseWtype
  reservedOp "~>"
  t2 <- parseWtype
  return $ TyCir t1 t2

parsePrimType :: Parser Type
parsePrimType = (whiteSpace >>) $
      parseTyUnit
  <|> parseTyBool
  <|> parseTyCir
  <|> parens parseType

binary :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String u Identity a
binary s f assoc = Ex.Infix (reservedOp s >> return f) assoc

typeOps :: [[Ex.Operator String u Identity Type]]
typeOps = [ [ binary "*" TyProd Ex.AssocLeft ]
          , [ binary "->" TyArr Ex.AssocRight] ]
            -- , binary "~>" TyCir Ex.AssocRight ] ]

parseType :: Parser Type
parseType = whiteSpace >> Ex.buildExpressionParser typeOps parsePrimType

----------------------------------------------------------------
-- Parse Circuit 
parseOutput :: Parser Circ
parseOutput = do
  reserved "output"
  pat <- parsePattern False
  return $ CcOutput pat

parseGateApp :: Parser Circ
parseGateApp = do
  pat2 <- parsePattern True
  reservedOp "<-"
  reserved "gate"
  gate <- parseGate
  pat1 <- parsePattern False
  semi
  ctx <- getState
  addPatNameBindingP pat2
  circ <- parseCirc
  setState ctx
  return $ CcGate pat2 gate pat1 circ

parseComp :: Parser Circ
parseComp = do
  pat <- parsePattern True
  reservedOp "<-"
  circ1 <- parseCirc
  semi
  ctx <- getState
  addPatNameBindingP pat
  circ2 <- parseCirc
  return $ CcComp pat circ1 circ2

-- TODO: buggy
parseCapp :: Parser Circ
parseCapp = do
  reserved "capp"
  t <- parseTerm
  reserved "to"
  p <- parsePattern False
  return $ CcApp t p

-- TODO: not finished

parseCirc :: Parser Circ
parseCirc = (whiteSpace >>) $
      parseOutput
  <|> parseCapp
  <|> try parseGateApp
  <|> try parseComp
  <|> parens parseCirc

----------------------------------------------------------------
-- Parse Circuit Type
parseOne :: Parser Wtype
parseOne = reservedOp "One" >> return WtUnit

parseBit :: Parser Wtype
parseBit = reservedOp "Bit" >> return WtBit

parseQubit :: Parser Wtype
parseQubit = reservedOp "Qubit" >> return WtQubit

parsePrimWtype :: Parser Wtype
parsePrimWtype = (whiteSpace >>) $
      parseOne
  <|> parseBit
  <|> parseQubit
  <|> parens parseWtype

wtypeOps :: [[Ex.Operator String u Identity Wtype]]
wtypeOps = [[binary "#" WtProd Ex.AssocLeft]]

parseWtype :: Parser Wtype
parseWtype = whiteSpace >> Ex.buildExpressionParser wtypeOps parsePrimWtype


----------------------------------------------------------------
-- Parse Pattern

parseWVar :: Bool -> Parser Pattern
parseWVar b = do
  var <- identifier
  if b then
    return $ PtName var
  else do
    ctx <- getState
    let idx = name2index ctx var
    return $ PtVar idx (length ctx)

parseEmpty :: Bool -> Parser Pattern
parseEmpty b = do
  reservedOp "()"
  return PtEmp

parsePProd :: Bool -> Parser Pattern
parsePProd b = parens $ do
  p1 <- parsePattern b
  comma
  p2 <- parsePattern b
  return $ PtProd p1 p2

-- | Parse a Pattern
-- True: use string name
-- False: use Debruijn Index
parsePattern :: Bool -> Parser Pattern
parsePattern b = (whiteSpace >>) $
      parseWVar b
  <|> try (parseEmpty b)
  <|> try (parsePProd b)

----------------------------------------------------------------
-- Parse Gate

-- parseNew0 :: Parser Gate
-- parseNew0 = reserved "new0" >> return GtNew0

-- parseNew1 :: Parser Gate
-- parseNew1 = reserved "new1" >> return GtNew1

-- parseInit0 :: Parser Gate
-- parseInit0 = reserved "init0" >> return GtInit0

-- parseInit1 :: Parser Gate
-- parseInit1 = reserved "init1" >> return GtInit1

-- parseMeas :: Parser Gate
-- parseMeas = reserved "meas" >> return GtMeas

-- parseDiscard :: Parser Gate
-- parseDiscard = reserved "discard" >> return GtDiscard

parseGate :: Parser Gate
parseGate = foldl (\p s -> p <|> (reserved s >> return (Gt s))) empty gateNames

----------------------------------------------------------------
runMyParser :: String -> Either ParseError Term
runMyParser = runParser parseTerm emptyctx "<CandyQwQ>" 

-- runMyParser :: String -> Either ParseError Wtype
-- runMyParser = runParser parseWtype emptyctx "<CandyQwQ>" 

-- runMyParser :: String -> Either ParseError Type
-- runMyParser = runParser parseType emptyctx "<CandyQwQ>" 
