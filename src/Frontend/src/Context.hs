module Context where

import Syntax

import Data.List

data Binding
  = NameBind
  | VarBind Type
  -- CirBind WireType

type Context = [(String, Binding)]

emptyctx :: Context
emptyctx = []

addBinding :: Context -> (String, Binding) -> Context
addBinding ctx (x, bind) = (x, bind) : ctx

name2index :: Monad m => Context -> String -> m Int
name2index ctx name =
  case findIndex ((== name). fst) ctx of
    Just ind -> return ind
    Nothing -> error $ "Unbounded variable name \"" ++ name ++ "\""