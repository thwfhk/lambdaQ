module Lexer where

import Text.Parsec (Parsec)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity ( Identity )

import Syntax

langDef :: Tok.LanguageDef st
langDef = emptyDef
    { Tok.commentLine = "QwQ"
    , Tok.reservedOpNames = ops
    , Tok.reservedNames = names
    }
  where
    ops = [ "*"
          , "->"
          , "~>"
          , "\\" -- lambda
          , "/"  -- kappa
          , "#" -- times
          , "<-"
          , "<="
          , "()"
          ]
    names = [ "unit"
            , "true"
            , "false"
            , "if"
            , "then"
            , "else"
            , "run"
            , "Unit"
            , "Bool"
            , "One"
            , "Bit"
            , "Qubit"
            , "output"
            , "gate"
            , "lift"
            , "capp", "to"
            ] ++ gateNames

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser langDef

comma :: Parsec String u String
comma = Tok.comma lexer

dot :: Parsec String u String
dot = Tok.dot lexer

colon :: Parsec String u String
colon = Tok.colon lexer

semi :: Parsec String u String
semi = Tok.semi lexer

parens :: Parsec String u a -> Parsec String u a
parens = Tok.parens lexer


whiteSpace :: Parsec String u ()
whiteSpace = Tok.whiteSpace lexer
-- commaSep :: Parser a -> Parser [a]
-- commaSep = Tok.commaSep lexer

-- semiSep :: Parser a -> Parser [a]
-- semiSep = Tok.semiSep lexer

identifier :: Parsec String u String
identifier = Tok.identifier lexer

reserved :: String -> Parsec String u ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parsec String u ()
reservedOp = Tok.reservedOp lexer