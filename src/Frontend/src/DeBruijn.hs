{-# LANGUAGE FlexibleContexts #-}
module DeBruijn where

import Syntax
import Context
import QASMSyntax
import Data.List
import Control.Monad.State
import Control.Monad.Except

-- Convert Circuit to De Bruijn Representation

-- | Transform a pattern to De Bruijn Index Representation
patDeBruijn :: MonadError Err m => Context -> Pattern -> m Pattern
patDeBruijn ctx (PtName var) = case name2index ctx var of
  Right idx -> return $ PtVar idx (length ctx)
  Left e -> error e
patDeBruijn ctx (PtEmp) = return PtEmp
patDeBruijn ctx (PtProd p1 p2) = do
  p1' <- patDeBruijn ctx p1
  p2' <- patDeBruijn ctx p2
  return $ PtProd p1' p2'

-- | Transform a circuit to De Bruijn Representation
circDeBruijn :: Circ -> ExceptT Err (State Context) Circ
circDeBruijn (CcOutput p) = do
  omega <- get
  p' <- patDeBruijn omega p
  return $ CcOutput p'
circDeBruijn (CcGate p1 g p2 c) = do
  omega <- get
  p2' <- patDeBruijn omega p2
  omega' <- addPatNameBinding omega p1
  put omega'
  c' <- circDeBruijn c
  put omega
  return $ CcGate p1 g p2' c'
circDeBruijn (CcComp p c1 c2) = do
  omega <- get
  c1' <- circDeBruijn c1
  omega' <- addPatNameBinding omega p
  put omega'
  c2' <- circDeBruijn c2
  put omega
  return $ CcComp p c1' c2'
circDeBruijn (CcApp t p) = do
  t' <- passThrough t
  return $ CcApp t' p
  where
    passThrough :: Term -> ExceptT Err (State Context) Term
    passThrough (TmCir p wt c) = do
      omega <- get
      omega' <- addPatNameBinding omega p
      put omega'
      c' <- circDeBruijn c
      put omega
      return $ TmCir p wt c'
    passThrough (TmRun c) = do
      c' <- circDeBruijn c
      return $ TmRun c'
    passThrough t = mapThrough passThrough t

mapThrough :: Monad m => (Term -> m Term) -> Term -> m Term
mapThrough f t = case t of
  TmAbs s ty tm -> do
    tm' <- f tm
    return $ TmAbs s ty tm'
  TmApp t1 t2 -> do
    t1' <- f t1
    t2' <- f t2
    return $ TmApp t1' t2'
  TmProd t1 t2 -> do
    t1' <- f t1
    t2' <- f t2
    return $ TmProd t1' t2'
  TmFst tm -> do
    tm' <- f tm
    return $ TmFst tm'
  TmSnd tm -> do
    tm' <- f tm
    return $ TmSnd tm'
  TmIf t1 t2 t3 -> do
    t1' <- f t1
    t2' <- f t2
    t3' <- f t3
    return $ TmIf t1 t2 t3