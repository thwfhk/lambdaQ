module Syntax where

type Name = String

data Type
  = TyUnit
  | TyBool
  | TyProd Type Type
  | TyArr Type Type
  -- TyCir Type Type
  deriving Show

data Term
  = TmVar Int Int 
  | TmUnit
  | TmTrue
  | TmFalse
  | TmAbs Name Type Term
  | TmApp Term Term
  | TmProd Term Term
  | TmFst Term
  | TmSnd Term
  | TmIf Term Term Term
  -- TmRun Circ
  -- TmCir String
  deriving Show

type Context = [(String, Type)]

