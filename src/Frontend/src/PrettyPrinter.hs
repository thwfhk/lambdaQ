module PrettyPrinter where

import Syntax
import Context
import QASMSyntax
import Data.List
import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------------
printType :: Type -> String
printType TyUnit = "Unit"
printType TyBool = "Bool"
printType (TyProd ty1 ty2) = printType ty1 ++ " * " ++ printType ty2
printType (TyArr ty1 ty2)  = printType ty1 ++ " -> " ++ printType ty2
printType (TyCir wty1 wty2)   = printWtype wty1 ++ " ~> " ++ printWtype wty2

printWtype :: Wtype -> String
printWtype WtUnit = "1"
printWtype WtBit = "Bit"
printWtype WtQubit = "Qubit"
printWtype (WtProd wty1 wty2) = printWtype wty1 ++ " ⊗ " ++ printWtype wty2

----------------------------------------------------------------

printQASM :: Program -> String
printQASM = foldr (\stmt s -> printStmt stmt ++ "\n" ++ s) ""

printStmt :: Statement -> String
printStmt (SmDecl decl) = case decl of
  Qreg s -> "qreg " ++ s ++ "[1];"
  Creg s -> "creg " ++ s ++ "[1];"
printStmt (SmQop qop) = printQop qop
printStmt (SmIf s n qop) = "if (" ++ s ++ " == " ++ show n ++ ") " ++ printQop qop

printQop :: Qop -> String
printQop qop = case qop of
  Uop uop -> printUop uop
  Measure q c -> "measure " ++ q ++ " -> " ++ c ++ ";"
  Reset s -> "reser " ++ s ++ ";"

printUop :: Uop -> String
printUop uop = case uop of
  UCX s1 s2 -> "CX " ++ s1 ++ ", " ++ s2 ++ ";"
  UX s -> "X " ++ s ++ ";"
  UH s -> "H " ++ s ++ ";"
  UZ s -> "Z " ++ s ++ ";"