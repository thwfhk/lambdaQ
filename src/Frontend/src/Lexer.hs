module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

langDef :: Tok.LanguageDef st
langDef = emptyDef
    { Tok.commentLine = "#"
    , Tok.reservedOpNames = ops
    , Tok.reservedNames = names
    }
  where
    ops = [ "*"
          , "->"
          , "~>"
          , "\\" -- lambda
          , "/"  -- kappa
          ]
    names = [ "unit"
            , "true"
            , "false"
            , "if"
            , "then"
            , "else"
            , "run"
            ]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

comma :: Parser String
comma = Tok.comma lexer

dot :: Parser String
dot = Tok.dot lexer

colon :: Parser String
colon = Tok.colon lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- commaSep :: Parser a -> Parser [a]
-- commaSep = Tok.commaSep lexer

-- semiSep :: Parser a -> Parser [a]
-- semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer