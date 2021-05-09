{-# LANGUAGE FlexibleContexts #-}
module TypeChecker where

import Syntax
import Context
import Data.List
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace

getGateType :: MonadError Err m => Gate-> m (Wtype, Wtype)
getGateType (Gt s) = case (find (\(a, _, _) -> a == s) gateInfos) of
  Nothing -> throwError $ "getGateWtype: gate " ++ s ++ " not defined" 
  Just (_, w1, w2) -> return $ (w1, w2)

wtype2type :: Wtype -> Type
wtype2type WtUnit = TyUnit
wtype2type WtBit = TyBool
wtype2type WtQubit = TyBool
wtype2type (WtProd wt1 wt2) = TyProd (wtype2type wt1) (wtype2type wt2)

typeOf :: Term -> ExceptT String (State Contexts) Type
typeOf TmUnit = return TyUnit
typeOf TmTrue = return TyBool
typeOf TmFalse = return TyBool
typeOf (TmVar x _) = do
  bty <- snd <$> index2Gamma x
  case bty of
    VarBind ty -> return ty
    _ -> throwError $ "typeof TmVar: not a VarBind"
typeOf (TmAbs var ty tm) = do
  ctx <- getfst
  setfst $ addBinding ctx (var, VarBind ty)
  ty2 <- typeOf tm
  return $ TyArr ty ty2
typeOf (TmApp t1 t2) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  case ty1 of
    TyArr ty11 ty12 ->
      if ty11 == ty2 then return ty12
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
  ctx <- getsnd
  addPatWtypeBinding ctx (p, wt) >>= setsnd
  wtc <- wtypeOf c
  return $ TyCir wt wtc

namesInPattern :: Pattern -> [Name]
namesInPattern (PtName s) = [s]
namesInPattern PtEmp = []
namesInPattern (PtProd p1 p2) = namesInPattern p1 ++ namesInPattern p2

-- | remove l2 from l1
remove :: Eq a => [a] -> [a] -> [a]
remove l1 l2 = filter (\x -> x `notElem` l2) l1

-- | check if l1 is a subset of l2
subset :: Eq a => [a] -> [a] -> Bool
li1 `subset` li2 = foldr (\x b -> x `elem` li2 && b) True li1

-- | check if l1 is equal to l2 (as a set)
equal :: Eq a => [a] -> [a] -> Bool
equal l1 l2 = l1 `subset` l2 && l2 `subset` l1

constructOmega :: MonadError Err m => [Name] -> Context -> m Context
constructOmega li omega = mapM f li
  where
    f x = case find ((==x) . fst) omega of
      Just t -> return t
      Nothing -> throwError $ "name " ++ show x ++ " not in omega"

constructOmega' :: MonadError Err m => Pattern -> Wtype -> Context -> m Context
constructOmega' pat wt ctx = case pat of
  PtVar _ _ -> throwError $ "constructOmega': Invalid PtVar in Pattern"
  PtEmp -> return ctx
  PtName name -> return $ addBinding ctx (name, WireBind wt)
  PtProd p1 p2 -> case wt of
    WtProd w1 w2 -> do
      ctx' <- constructOmega' p1 w1 ctx
      constructOmega' p2 w2 ctx'
    _ -> throwError $ "constructOmega': Pattern and Wtype mismatch"

-- | get the names used in a circuit
getOmegaNames :: Circ -> [Name]
getOmegaNames (CcOutput p) = namesInPattern p
getOmegaNames (CcGate p1 g p2 c) =
  let li1 = namesInPattern p1 in
  let li = getOmegaNames c in
  let li2 = namesInPattern p2 in
  li1 ++ remove li li2
getOmegaNames (CcComp p c1 c2) =
  let li1 = getOmegaNames c1 in
  let li  = getOmegaNames c2 in
  let lip = namesInPattern p in
  li1 ++ (remove li lip)
getOmegaNames (CcApp t p) = namesInPattern p

omegaPatWtype :: Pattern -> ExceptT String (State Contexts) Wtype
omegaPatWtype (PtEmp) = return WtUnit
omegaPatWtype (PtName s) = do
  omega <- getsnd
  (_, bind) <- name2Omega s
  case bind of
    WireBind wt -> return wt
    _ -> throwError $ "not WireBind in omega"
omegaPatWtype (PtProd p1 p2) = do
  wt1 <- omegaPatWtype p1
  wt2 <- omegaPatWtype p2
  return $ WtProd wt1 wt2

-- opwCheck DO NOT use the omega in the state monad
opwCheck :: Context -> Pattern -> Wtype -> ExceptT String (State Contexts) ()
opwCheck omega p wt = do
  oldOmega <- getsnd
  setsnd omega
  wt' <- omegaPatWtype p
  setsnd oldOmega
  if wt' == wt then return ()
  else throwError $ "opwCheck error"

wtypeOf :: Circ -> ExceptT String (State Contexts) Wtype
-- wtypeOf = undefined
wtypeOf (CcOutput p) = do
  omega <- getsnd
  -- traceM $ "omega: " ++ show (map fst omega) -- NOTE: debug here
  -- traceM $ "li:    " ++ show (namesInPattern p)
  if map fst omega `equal` namesInPattern p
    then omegaPatWtype p
    else throwError $ "wtypeOf CcOutput: Omega mismatch"
wtypeOf (CcGate p2 g p1 c) = do
  omega <- getsnd
  (w1, w2) <- getGateType g
  omega1 <- constructOmega' p1 w1 emptyctx
  let omega' = omega `remove` omega1 -- already check omega == omega' + omega1
  omega2 <- constructOmega' p2 w2 emptyctx
  setsnd $ omega2 ++ omega'
  w <- wtypeOf c
  setsnd $ omega
  return w
wtypeOf (CcComp p c c') = do
  omega <- getsnd
  omega1 <- constructOmega (getOmegaNames c) omega
  let omega2 = omega `remove` omega1
  setsnd omega1
  w <- wtypeOf c
  omega' <- constructOmega' p w emptyctx
  setsnd $ omega2 ++ omega'
  w' <- wtypeOf c'
  setsnd $ omega
  return w'
wtypeOf (CcApp t p) = do
  omega <- getsnd
  setsnd emptyctx
  ty <- typeOf t
  setsnd omega
  case ty of
    TyCir w1 w2 -> do
      let li = namesInPattern p
      omega1 <- constructOmega li omega
      opwCheck omega1 p w1
      return w2
    _ -> throwError $ "wtypeOf CcApp: sar is stupid!!!"


