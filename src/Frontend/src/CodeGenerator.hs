{-# LANGUAGE FlexibleContexts #-}
module CodeGenerator where

import Syntax
import Context
import QASMSyntax
import Data.List
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

-- type Registers = [(Name, Name)] -- var name, register name
type Mapping = Map.Map Name Name
type Registers = (Mapping, Int)
emptyregs :: (Map.Map k a, Int)
emptyregs = (Map.empty, 0)

-- pickNewReg :: Registers -> Name
-- pickNewReg regs = "r" ++ show (snd regs)

-- | pick a new register name and increment the count
pickNewReg :: MonadState Registers m => m Name
pickNewReg = do
  (regs, cnt) <- get
  put (regs, cnt+1)
  return $ "r" ++ show cnt

-- | add a new mapping
addMapping :: MonadState Registers m => Name -> Name -> m ()
addMapping var reg = do
  (regs, cnt) <- get
  let regs' = Map.insert var reg regs
  put (regs', cnt)

-- | add new mappings
addMappings :: (MonadState Registers m, MonadError Err m) => Pattern -> Pattern -> m ()
addMappings PtEmp PtEmp = return ()
addMappings (PtName var) (PtName reg) = addMapping var reg
addMappings (PtProd pvar1 pvar2) (PtProd preg1 preg2) = do
  addMappings pvar1 preg1
  addMappings pvar2 preg2

-- | map variables to registers
var2reg :: Mapping -> Pattern -> Pattern
var2reg regs p = case p of
  PtEmp -> PtEmp
  PtName var -> PtName (regs Map.! var)
  PtProd p1 p2 -> PtProd (var2reg regs p1) (var2reg regs p2)

-- | restore the original mapping, preserve the new count
restore :: MonadState Registers m => Registers -> m ()
restore (regs, cnt) = do
  (regs', cnt') <- get
  put (regs, cnt')

-- NOTE: I think that using GADT to define Term is better
term2QASM :: Term -> ExceptT Err (State Registers) Program
term2QASM (TmCir p wt c) = case wt of
  WtUnit -> do
    (prog, _) <- circ2QASM c
    return prog
  _ -> throwError $ "circuit2QASM : Circuit needs arguments"
term2QASM _ = throwError $ "circuit2QASM : undefined currently"

circ2QASM :: Circ -> ExceptT Err (State Registers) (Program, Pattern) -- QASM Program and return value
circ2QASM (CcOutput p) = do
  x <- get
  (regs, _) <- get
  return ([], var2reg regs p)
circ2QASM (CcGate p1 g p2 c) = case g of
  Gt "new0" -> do
    current <- get
    creg <- pickNewReg
    let (PtName var) = p1 in addMapping var creg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmDecl (Creg creg)] ++ prog', outpat)
  Gt "new1" -> do
    current <- get
    creg <- pickNewReg
    qreg <- pickNewReg
    let (PtName var) = p1 in addMapping var creg
    (prog', outpat) <- circ2QASM c
    let prog =
          [ SmDecl (Creg creg)
          , SmDecl (Qreg qreg)
          , SmQop (Uop (UX qreg))
          , SmQop (Measure qreg creg)]
    restore current
    return (prog ++ prog', outpat)
  Gt "init0" -> do
    current <- get
    qreg <- pickNewReg
    let (PtName var) = p1 in addMapping var qreg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmDecl (Qreg qreg)] ++ prog', outpat)
  Gt "init1" -> do
    current <- get
    qreg <- pickNewReg
    let (PtName var) = p1 in addMapping var qreg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmDecl (Qreg qreg), SmQop (Uop (UX qreg))] ++ prog', outpat)
  Gt gatename | gatename `elem` singleUnitaryGates -> do
    current@(regs, cnt) <- get
    let (PtName reg) = var2reg regs p2
    let (PtName var) = p1 in addMapping var reg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmQop (Uop (getSingleUop gatename reg))] ++ prog', outpat)
  Gt "meas" -> do
    current@(regs, cnt) <- get
    let (PtName qreg) = var2reg regs p2
    creg <- pickNewReg
    let (PtName var) = p1 in addMapping var creg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmQop (Measure qreg creg)] ++ prog', outpat)
  Gt "discard" -> do
    circ2QASM c
  Gt "CNOT" -> do
    current@(regs, cnt) <- get
    let preg = var2reg regs p2
    let (PtProd (PtName reg1) (PtName reg2)) = preg -- needed by return
    let pvar = p1 in addMappings pvar preg
    (prog', outpat) <- circ2QASM c
    restore current
    return ([SmQop (Uop (UCX reg1 reg2))] ++ prog', outpat)
circ2QASM (CcComp p c1 c2) = do
  current <- get
  (prog1, outpat1) <- circ2QASM c1
  addMappings p outpat1
  (prog2, outpat2) <- circ2QASM c2
  return (prog1 ++ prog2, outpat2)
-- circ2QASM (CcApp t p') = case t of
--   TmCir _ _ _ -> tmCir2QASM t p'
--   TmIf _ _ _ -> tmIf2QASM t p'

-- tmIf2QASM :: Term -> Pattern -> ExceptT Err (State Registers) (Program, Pattern)
-- tmIf2QASM t p' = case t of
--   TmIf t1 t2 t3 -> case t1 of
--       TmG s -> do
--         current <- get
--         (prog2, outpat2) <- tmCir2QASM t2 p'
--         (prog3, outpat3) <- tmCir2QASM t3 p'
--         qop2 <- checkIf prog2
--         qop3 <- checkIf prog3
--         restore current
--         if outpat2 /= outpat3 -- outpat2 and outpat3 need to be the same
--           then throwError $ "tmIf2QASM : different output patterns"
--           else return ([SmIf s 1 qop2, SmIf s 0 qop3], outpat2)
--       TmNot (TmG s) -> do
--         current <- get
--         (prog2, outpat2) <- tmCir2QASM t2 p'
--         (prog3, outpat3) <- tmCir2QASM t3 p'
--         qop2 <- checkIf prog2
--         qop3 <- checkIf prog3
--         restore current
--         if outpat2 /= outpat3 -- outpat2 and outpat3 need to be the same
--           then throwError $ "tmIf2QASM : different output patterns"
--           else return ([SmIf s 0 qop2, SmIf s 1 qop3], outpat2)
--       _ -> throwError $ "tmIf2QASM : condition is too complex"
--   _ -> throwError $ "tmIf2QASM : not a if"
--   where
--     checkIf :: (MonadState Registers m, MonadError Err m) => Program -> m Qop
--     checkIf prog = case prog of
--       [SmQop qop] -> return qop
--       _ -> throwError $ "tmIf2QASM : two many statements in if-branch"

-- tmCir2QASM :: Term -> Pattern -> ExceptT Err (State Registers) (Program, Pattern)
-- tmCir2QASM t p' = case t of
--   TmCir p _ c -> do
--     current@(regs, cnt) <- get
--     let preg = var2reg regs p'
--     addMappings p preg
--     (prog', outpat) <- circ2QASM c
--     restore current
--     return (prog', outpat)
--   _ -> throwError $ "tmCir2QASM : not a circuit abstraction"