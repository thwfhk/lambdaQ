module Syntax where

data Type
  = TyUnit
  | TyBool
  | TyProd Type Type
  | TyArr Type Type
  | TyCir Type Type
  deriving Show

data Term
  = TmVar Int Int 
  | TmUnit
  | TmTrue
  | TmFalse
  | TmAbs String Type Term
  | TmApp Term Term
  | TmProd Term Term
  | TmFst Term
  | TmSnd Term
  | TmIf Term Term Term
  | TmRun Circ
  | TmCir Pattern Wtype Circ
  deriving Show

data Circ
  = CcOutput Pattern
  | CcGate Pattern Gate Pattern Circ
  | CcComp Pattern Circ Circ
  | CcLift Term Pattern Circ
  | CcApp Term Pattern
  deriving Show

data Wtype
  = WtUnit
  | WtBit
  | WtQubit
  | WtProd Wtype Wtype
  deriving Show

data Pattern
  = PtVar Int Int -- De Bruijn Index
  | PtName String -- Name
  | PtEmp
  | PtProd Pattern Pattern
  deriving (Show, Eq)

data Gate = Gt String
  -- = GtNew0 | GtNew1 | GtInit0 | GtInit1 | GtMeas | GtDiscard
  deriving Show

gateNames :: [String]
gateNames = ["new0", "new1", "init0", "init1", "meas", "discard"]