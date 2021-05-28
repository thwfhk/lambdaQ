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

getFst :: Parser Context
getFst = fst <$> getState
getSnd :: Parser Context
getSnd = fst <$> getState
setFst :: Context -> Parser ()
setFst ctx = getState >>= \(_, omega) -> setState (ctx, omega)
setSnd :: Context -> Parser ()
setSnd ctx = getState >>= \(gamma, _) -> setState (gamma, ctx)

----------------------------------------------------------------
-- Parse Command

parseDef :: Parser Command
parseDef = do
  reserved "fun"
  x <- identifier
  reservedOp "="
  t <- parseTerm
  ctx <- getFst
  setFst $ addBinding ctx (x, NameBind)
  return $ Def x t

-- parseAno :: Parser Command
-- parseAno = do
--   reserved ""
--   t <- parseTerm
--   return $ Ano t

parseCommand :: Parser Command
parseCommand = whiteSpace >> parseDef

parseCommands :: Parser [Command]
parseCommands = (whiteSpace >>) $
      (do
        s <- getInput
        -- traceM ("HI: " ++ show s)
        cmd <- parseCommand
        cmds <- parseCommands
        return $ cmd : cmds)
  <|> return []

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

parseNot :: Parser Term
parseNot = do
  reserved "Not"
  t <- parseTerm
  return $ TmNot t

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
    _ -> error "parseProj: Product Index Error" -- TODO: any better way?

parseVar :: Parser Term
parseVar = do
  var <- identifier
  ctx <- getFst
  case name2index ctx var of -- the only use of context during parsing
    Right idx -> return $ TmVar idx var
    Left e -> error e

parseAbs :: Parser Term
parseAbs = do
  reservedOp "\\"
  var <- identifier
  colon
  ty <- parseType
  dot
  ctx <- getFst
  -- setFst $ addBinding ctx (var, VarBind ty)
  setFst $ addBinding ctx (var, NameBind) -- NameBind is enough for parsing
  term <- parseTerm
  setFst ctx
  return $ TmAbs var ty term

parseRun :: Parser Term
parseRun = do
  reserved "run"
  circ <- parseCirc
  return $ TmRun circ

parseCAbs :: Parser Term
parseCAbs = do
  reservedOp "/"
  patvar <- parsePattern
  colon
  wtype <- parseWtype
  dot
  ctx <- getSnd
  case addPatNameBinding ctx patvar of
    Right ctx' -> setSnd ctx'
    Left e -> error e
  circ <- parseCirc
  setSnd ctx
  return $ TmCir patvar wtype circ

parsePrimTerm :: Parser Term -- without projection
parsePrimTerm = (whiteSpace >>) $
      unit
  <|> true
  <|> false
  <|> parseIf
  <|> try parseVar
  <|> parseAbs
  <|> try parseProd
  <|> parseRun
  <|> parseCAbs
  <|> parens parseTerm

termOps :: [[Ex.Operator String Contexts Identity Term]]
termOps = [ [Ex.Postfix parseProj] ]

parseTermExpr :: Parser Term -- deal with projection
parseTermExpr = whiteSpace >> Ex.buildExpressionParser termOps parsePrimTerm

parseTerm :: Parser Term -- all terms
parseTerm = whiteSpace >> chainl1 (try parseTermExpr) (return TmApp)

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
  <|> try parseTyCir
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
  ctx <- getSnd
  case addPatNameBinding ctx pat2 of
    Right ctx' -> setSnd ctx'
    Left e -> error e
  circ <- parseCirc
  setSnd ctx
  return $ CcGate pat2 gate pat1 circ

parseGateSugar :: Parser Circ
parseGateSugar = do
  reserved "gate"
  gate <- parseGate
  pat <- parsePattern
  return $ CcGateS gate pat

parseComp :: Parser Circ
parseComp = do
  pat <- parsePattern
  reservedOp "<-"
  circ1 <- parseCirc
  semi
  ctx <- getSnd
  case addPatNameBinding ctx pat of
    Right ctx' -> setSnd ctx'
    Left e -> error e
  circ2 <- parseCirc
  setSnd ctx
  return $ CcComp pat circ1 circ2

parseCapp :: Parser Circ
parseCapp = do
  reserved "capp"
  t <- parseTerm
  reserved "to"
  p <- parsePattern
  return $ CcApp t p

parseLift :: Parser Circ
parseLift = do
  var <- parsePattern
  reservedOp "<-|"
  reserved "lift"
  p <- parsePattern
  semi
  gamma <- getFst
  case addPatNameBinding gamma var of
    Right gamma' -> setFst gamma'
    Left e -> error e
  c <- parseCirc
  setFst gamma
  return $ CcLift var p c

-- parseLift :: Parser Circ
-- parseLift = do
--   var <- identifier
--   reservedOp "<-|"
--   reserved "lift"
--   p <- parsePattern
--   semi
--   gamma <- getFst
--   setFst $ addBinding gamma (var, NameBind)
--   c <- parseCirc
--   return $ CcLift var p c


parseCirc :: Parser Circ
parseCirc = (whiteSpace >>) $
      parseOutput
  <|> parseCapp
  <|> try parseLift
  <|> try parseGateApp
  <|> try parseComp
  <|> try parseGateSugar
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

parseWVar :: Parser Pattern
parseWVar = do
  var <- identifier
  return $ PtName var

parseEmpty :: Parser Pattern
parseEmpty = do
  reservedOp "()"
  return PtEmp

parsePProd :: Parser Pattern
parsePProd = parens $ do
  p1 <- parsePattern
  comma
  p2 <- parsePattern
  return $ PtProd p1 p2

-- | Parse a Pattern
parsePattern :: Parser Pattern
parsePattern = (whiteSpace >>) $
      try parseWVar
  <|> try parseEmpty
  <|> try parsePProd

----------------------------------------------------------------
-- Parse Gate

parseGate :: Parser Gate
parseGate = foldl (\p s -> p <|> (reserved s >> return (Gt s))) empty gateNames