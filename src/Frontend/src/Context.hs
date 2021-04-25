module Context where

import Syntax

import Data.List

type Name = String
-- data Name
--   = Var String
--   | Wir Pattern
--   deriving (Show, Eq)
data Binding
  = NameBind
  | VarBind Type
  | WireBind Wtype

type Context = [(Name, Binding)]

emptyctx :: Context
emptyctx = []

addBinding :: Context -> (Name, Binding) -> Context
addBinding ctx (x, bind) = (x, bind) : ctx

name2index :: Monad m => Context -> Name -> m Int
name2index ctx name =
  case findIndex ((== name). fst) ctx of
    Just ind -> return ind
    Nothing -> error $ "Unbounded variable name \"" ++ name ++ "\""