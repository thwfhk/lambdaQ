module QASMSyntax where

-- Simplified OpenQASM-2.0 Syntax

type Program = [Statement]

data Statement
  = SmDecl Decl                 -- <decl>
  | SmQop  Qop                  -- <qop>
  | SmIf String Int Statement   -- if (<id> == <nninteger>) <qop>
  deriving Show

data Decl
  = Qreg String  -- qreg <id> [<nninteger>]
  | Creg String  -- qreg <id> [<nninteger>]
  deriving Show

data Qop
  = Uop Uop                   -- <utop>
  | Measure Argument Argument -- measure <argument> -> <argument>
  | Reset Argument            -- resert <argument>
  deriving Show

data Uop
  = UCX Argument Argument  -- CX <argument>, <argument>
  | UX Argument
  | UH Argument
  deriving Show

type Argument = String
-- data Argument
--   = Id String     -- <id>
--   | Index String  -- <id> [<nninteger>]

-- some helper functions
getSingleUop :: String -> String -> Uop
getSingleUop "X" s = UX s
getSingleUop "H" s = UH s