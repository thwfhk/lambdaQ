{-# LANGUAGE FlexibleContexts #-}
module Context where

import Syntax
import Data.List
import Text.Parsec (ParsecT, getState, setState)
import Data.Functor.Identity (Identity)
import Control.Monad.State
import Control.Monad.Except
type Parser a = ParsecT String Context Identity a

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

type Err = String

emptyctx :: Context
emptyctx = []

setQwQ :: MonadState c m => (c -> a -> c) -> (a -> m ())
setQwQ f a = do
  ctx <- get
  put $ f ctx a

setQwQParser :: Monad m => (c -> a -> c) -> (a -> ParsecT s c m ())
setQwQParser f a = do
  ctx <- getState
  setState $ f ctx a

addBinding :: Context -> (Name, Binding) -> Context
addBinding ctx (x, bind) = (x, bind) : ctx

addBindingM :: MonadState Context m => (Name, Binding) -> m ()
addBindingM = setQwQ addBinding

addBindingP :: (Name, Binding) -> Parser ()
addBindingP = setQwQParser addBinding

-- | Add all wire variables in the Pattern to the Context
-- The types of all variables are given in the corresponding position of Wtype
addPatWtypeBinding :: Context -> (Pattern, Wtype) -> Context
addPatWtypeBinding ctx (patvar, wtype) = case patvar of
  PtVar _ _    -> error "Invalid PtVar in addPatBinding2State"
  PtName name  -> addBinding ctx (name, WireBind wtype)
  PtEmp        -> emptyctx
  PtProd p1 p2 -> case wtype of
    WtProd w1 w2 -> let ctx' = addPatWtypeBinding ctx (p1, w1)
                    in addPatWtypeBinding ctx' (p2, w2)
    _            -> error "Pattern and Wtype not match in addPatBinding2State"

addPatNameBinding :: Context -> Pattern -> Context
addPatNameBinding ctx patvar = case patvar of
  PtVar _ _    -> error "Invalid PtVar in addPatBinding2State"
  PtName name  -> addBinding ctx (name, NameBind)
  PtEmp        -> emptyctx
  PtProd p1 p2 -> let ctx' = addPatNameBinding ctx p1
                  in addPatNameBinding ctx' p2

addPatWtypeBindingP :: (Pattern, Wtype) -> Parser ()
addPatWtypeBindingP = setQwQParser addPatWtypeBinding

addPatNameBindingP :: Pattern -> Parser ()
addPatNameBindingP = setQwQParser addPatNameBinding

addPatWtypeBindingM :: MonadState Context m => (Pattern, Wtype) -> m ()
addPatWtypeBindingM = setQwQ addPatWtypeBinding

addPatNameBindingM :: MonadState Context m => Pattern -> m ()
addPatNameBindingM = setQwQ addPatNameBinding
-- TODO: use MonadError to rewrite these

-- addPatWtypeBinding :: Pattern -> Wtype -> Parser ()
-- addPatWtypeBinding patvar wtype = case patvar of
--   PtVar _ _    -> error "Invalid PtVar in addPatBinding2State"
--   PtName name  -> getState >>= \ctx -> setState (addBinding ctx (name, WireBind wtype))
--   PtEmp        -> return ()
--   PtProd p1 p2 -> case wtype of
--     WtProd w1 w2 -> addPatWtypeBinding p1 w1 >> addPatWtypeBinding p2 w2
--     _            -> error "Pattern and Wtype not match in addPatBinding2State"

-- addPatNameBinding :: Pattern -> Parser ()
-- addPatNameBinding patvar = case patvar of
--   PtVar _ _    -> error "Invalid PtVar in addPatBinding2State"
--   PtName name  -> getState >>= \ctx -> setState (addBinding ctx (name, NameBind))
--   PtEmp        -> return ()
--   PtProd p1 p2 -> addPatNameBinding p1 >> addPatNameBinding p2

name2index :: Context -> Name -> Int
name2index ctx name =
  case findIndex ((== name). fst) ctx of
    Just ind -> ind
    Nothing -> error $ "Unbounded variable name \"" ++ name ++ "\""

index2entry :: Context -> Int -> Either Err (Name, Binding)
index2entry ctx x
  | length ctx > x = Right $ ctx !! x
  | otherwise = Left $ "getIndex: index " ++ show x
              ++ "is overflow, current length: " ++ show (length ctx)

index2bind :: Context -> Int -> Either Err Binding
index2bind ctx x = fmap snd $ index2entry ctx x

getQwQ :: (MonadState c m, MonadError Err m) 
       => (c -> a -> Either Err b) -> a -> m b
getQwQ f a = do
  ctx <- get
  case f ctx a of
    Right b -> return b
    Left e -> throwError e

index2entryM :: (MonadState Context m, MonadError Err m) => Int -> m (Name, Binding)
index2entryM = getQwQ index2entry

index2bindM :: MonadState Context m => Int -> ExceptT String m Binding
index2bindM = getQwQ index2bind