{-# LANGUAGE FlexibleContexts #-}
module TypeChecker where

import Syntax
import Context
import Data.List
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace

-- | get the wtype of a gate
getGateType :: MonadError Err m => Gate-> m (Wtype, Wtype)
getGateType (Gt s) = case (find (\(a, _, _) -> a == s) gateInfos) of
  Nothing -> throwError $ "gate " ++ s ++ " not defined" 
  Just (_, w1, w2) -> return $ (w1, w2)

-- | lift a wtype to a type
wtype2type :: Wtype -> Type
wtype2type WtUnit = TyUnit
wtype2type WtBit = TyBool
wtype2type WtQubit = TyBool
wtype2type (WtProd wt1 wt2) = TyProd (wtype2type wt1) (wtype2type wt2)

-- | type inference for Γ |- t : T
typeOf :: Term -> ExceptT String (State Contexts) Type
typeOf TmUnit = return TyUnit
typeOf TmTrue = return TyBool
typeOf TmFalse = return TyBool
typeOf (TmVar x _) = do
  bty <- snd <$> index2entryGamma x
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
      else throwError $ "typeOf TmApp: parameter type not match"
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
typeOf (TmNot t) = do
  ty <- typeOf t
  if ty /= TyBool then throwError $ "typeOf TmNot: not a boolean"
  else return TyBool
typeOf (TmIf t1 t2 t3) = do
  ty1 <- typeOf t1
  if ty1 /= TyBool then throwError $ "typeOf TmIf: condition is not a boolean"
  else do
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    if ty2 /= ty3 then throwError $ "typeOf TmIf: branches have different types"
    else return ty2
typeOf (TmRun c) = do
  wt <- wtypeOf c
  return $ wtype2type wt
typeOf (TmCir p wt c) = do
  ctx <- getsnd
  addPatWtypeBinding ctx (p, wt) >>= setsnd
  wtc <- wtypeOf c
  return $ TyCir wt wtc

-- some utilities functions for operating lists
-- | remove l2 from l1
remove :: Eq a => [a] -> [a] -> [a]
remove l1 l2 = filter (\x -> x `notElem` l2) l1

-- | check if l1 is a subset of l2
subset :: Eq a => [a] -> [a] -> Bool
li1 `subset` li2 = foldr (\x b -> x `elem` li2 && b) True li1

-- | check if l1 is equal to l2 (as a set)
equal :: Eq a => [a] -> [a] -> Bool
equal l1 l2 = l1 `subset` l2 && l2 `subset` l1

exactlyEqual :: (Eq a, Ord a) => [a] -> [a] -> Bool
exactlyEqual l1 l2 =
  let l1' = sort l1 in
  let l2' = sort l2 in
  l1' == l2'

-- | extract bindings from an omega w.r.t. a list of names
extractOmega :: MonadError Err m => [Name] -> Context -> m Context
extractOmega li omega = mapM f li
  where
    -- f returns the leftmost (newest) binding
    f x = case find ((==x) . fst) omega of
      Just t -> return t
      Nothing -> throwError $ "extractOmega: name " ++ show x ++ " is not in omega"

-- | get the names used in a pattern
namesInPattern :: Pattern -> [Name]
namesInPattern (PtName s) = [s]
namesInPattern PtEmp = []
namesInPattern (PtProd p1 p2) = namesInPattern p1 ++ namesInPattern p2

-- | get the names used in a circuit
getOmegaNames :: Circ -> [Name]
getOmegaNames (CcOutput p) = namesInPattern p
getOmegaNames (CcGate p1 g p2 c) =
  let li1 = namesInPattern p1 in
  let li2 = namesInPattern p2 in
  let lic = getOmegaNames c in
  li1 ++ remove lic li2
getOmegaNames (CcComp p c1 c2) =
  let li1 = getOmegaNames c1 in
  let lip = namesInPattern p in
  let li2 = getOmegaNames c2 in
  li1 ++ (remove li2 lip)
getOmegaNames (CcLift t p c) = namesInPattern p ++ getOmegaNames c
getOmegaNames (CcApp t p) = namesInPattern p

-- | type inference for Omega |- p : w
-- It checks whether p uses all variables in omega exactly once.
patWtypeOf :: Pattern -> ExceptT String (State Contexts) Wtype
patWtypeOf p = do
    omega <- getsnd
    wt <- pwtof p
    if fmap fst omega `exactlyEqual` namesInPattern p
      then return wt
      else throwError $ "patWtypeOf: linearity condition failed"
  where
    pwtof :: Pattern -> ExceptT String (State Contexts) Wtype
    pwtof (PtEmp) = return WtUnit
    pwtof (PtName s) = do
      omega <- getsnd
      (_, bind) <- name2entryOmega s
      case bind of
        WireBind wt -> return wt
        _ -> throwError $ "patWtypeOf: not a WireBind in omega"
    pwtof (PtProd p1 p2) = do
      wt1 <- pwtof p1
      wt2 <- pwtof p2
      return $ WtProd wt1 wt2

-- | type check Omega |- p : w
opwCheck :: Pattern -> Wtype -> ExceptT String (State Contexts) ()
opwCheck p wt = do
  omega <- getsnd
  wt' <- patWtypeOf p
  if wt' == wt
    then return ()
    else throwError $ "opwCheck error"

-- | type inference for Γ ; Ω |- c : W
wtypeOf :: Circ -> ExceptT String (State Contexts) Wtype
wtypeOf (CcOutput p) = do
  omega <- getsnd
  patWtypeOf p
wtypeOf (CcGate p2 g p1 c) = do
  omega <- getsnd
  (w1, w2) <- getGateType g
  omega1 <- constructOmega p1 w1 emptyctx
  let omega' = omega `remove` omega1 -- omega == omega' + omega1
  omega2 <- constructOmega p2 w2 emptyctx
  setsnd $ omega2 ++ omega'
  w <- wtypeOf c
  setsnd $ omega
  return w
wtypeOf (CcComp p c c') = do
  omega <- getsnd
  omega1 <- extractOmega (getOmegaNames c) omega
  let omega2 = omega `remove` omega1 -- omega == omega1 + omega2
  setsnd omega1
  w <- wtypeOf c
  omega' <- constructOmega p w emptyctx -- omega' |- p : w
  setsnd $ omega2 ++ omega'
  w' <- wtypeOf c'
  setsnd $ omega
  return w'
wtypeOf (CcLift x p c) = do
  omega <- getsnd
  omegap <- extractOmega (namesInPattern p) omega
  setsnd omegap
  wtype <- patWtypeOf p
  setsnd $ omega `remove` omegap -- omega == omegap + omegac
  gamma <- getfst
  addPatTypeBinding gamma (x, (wtype2type wtype)) >>= setfst
  -- setfst $ addBinding gamma (x, VarBind (wtype2type wtype))
  wtype' <- wtypeOf c
  put (gamma, omega)
  return wtype'
wtypeOf (CcApp t p) = do
  omega <- getsnd
  setsnd emptyctx -- omega needs to be cleared here
  ty <- typeOf t
  setsnd omega
  case ty of
    TyCir w1 w2 -> do
      opwCheck p w1
      return w2
    _ -> throwError $ "wtypeOf CcApp: not a circuit function"


