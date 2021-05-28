module Desugar where

import Syntax

deGate :: Circ -> Circ
deGate (CcGateS gate pat) = CcGate pat gate pat (CcOutput pat)
deGate c = c

mapCirc :: (Term -> Term, Circ -> Circ) -> Circ -> Circ
mapCirc f c = case (snd f c) of
  CcGate p1 g p2 c -> CcGate p1 g p2 (mapCirc f c)
  CcComp p c1 c2 -> CcComp p (mapCirc f c1) (mapCirc f c2)
  CcLift p1 p2 c -> CcLift p1 p2 (mapCirc f c)
  CcApp t p -> CcApp (mapTerm f t) p
  x -> x

mapTerm :: (Term -> Term, Circ -> Circ) -> Term -> Term
mapTerm f t = case (fst f t) of
  TmNot t -> TmNot (mapTerm f t)
  TmAbs s ty t -> TmAbs s ty (mapTerm f t)
  TmApp t1 t2 -> TmApp (mapTerm f t1) (mapTerm f t2)
  TmProd t1 t2 -> TmProd (mapTerm f t1) (mapTerm f t2)
  TmFst t -> TmFst (mapTerm f t)
  TmSnd t -> TmSnd (mapTerm f t)
  TmIf t1 t2 t3 -> TmIf (mapTerm f t1) (mapTerm f t2) (mapTerm f t3)
  TmRun c -> TmRun (mapCirc f c)
  TmCir p wty c -> TmCir p wty (mapCirc f c)
  x -> x

desugar :: Term -> Term
desugar = mapTerm (id, deGate)