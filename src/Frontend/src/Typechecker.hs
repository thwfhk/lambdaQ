module Typechecker where

import Syntax
import Context
import Data.List
import Control.Monad.State
import Control.Monad.Except

getGateType :: Gate-> Wtype
getGateType (Gt s) = case (find (\(a, _, _) -> a == s) gateInfos) of
  Nothing -> error $ "getGateWtype: gate " ++ s ++ " not defined" 
  Just (_, _, c) -> c

wtype2type :: Wtype -> Type
wtype2type WtUnit = TyUnit
wtype2type WtBit = TyBool
wtype2type WtQubit = TyBool
wtype2type (WtProd wt1 wt2) = TyProd (wtype2type wt1) (wtype2type wt2)

typeOf :: Term -> ExceptT String (State Context) Type
typeOf TmUnit = return TyUnit
typeOf TmTrue = return TyBool
typeOf TmFalse = return TyBool
typeOf (TmVar x _) = do
  bty <- index2bindM x
  case bty of
    VarBind ty -> return ty
    _ -> throwError $ "typeof TmVar: not a VarBind"
typeOf (TmAbs var ty tm) = do
  addBindingM (var, VarBind ty)
  ty2 <- typeOf tm
  return $ TyArr ty ty2
typeOf (TmApp t1 t2) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  case ty1 of
    TyArr ty11 ty12 ->
      if ty11 == ty2 then return ty2
      else throwError $ "typeOf TmApp: type not match"
    _ -> throwError $ "typeOf TmApp: not a function"
typeOf (TmProd t1 t2) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  return $ TyProd ty1 ty2
typeOf (TmFst t) = do
  ty <- typeOf t
  case ty of
    TyProd ty1 _ -> return ty1
    _ -> throwError $ "typeOf TmFst: not a product"
typeOf (TmSnd t) = do
  ty <- typeOf t
  case ty of
    TyProd _ ty2 -> return ty2
    _ -> throwError $ "typeOf TmSnd: not a product"
typeOf (TmIf t1 t2 t3) = do
  ty1 <- typeOf t1
  if ty1 /= TyBool then throwError $ "typeOf TmIf: condition is not a boolean"
  else do
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    if ty2 /= ty3 then throwError $ "typeOf TmIf: different types on branches"
    else return ty2
typeOf (TmRun c) = do
  wt <- wtypeOf c
  return $ wtype2type wt
typeOf (TmCir p wt c) = do
  -- wtp <- wtypeOfPattern p
  -- if wtp /= wt then throwError $ "typeOf TmCir: pattern type not match"
  addPatWtypeBindingM (p, wt)
  wtc <- wtypeOf c
  return $ TyCir wt wtc


wtypeOf :: Circ -> ExceptT String (State Context) Wtype
wtypeOf = undefined

wtypeOfPattern :: Pattern -> ExceptT String (State Context) Wtype
wtypeOfPattern = undefined



