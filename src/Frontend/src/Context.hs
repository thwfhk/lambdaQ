{-# LANGUAGE FlexibleContexts #-}
module Context where

import Syntax
import Data.List
import Text.Parsec (ParsecT, getState, setState)
import Data.Functor.Identity (Identity)
import Control.Monad.State
import Control.Monad.Except
type Parser a = ParsecT String Contexts Identity a

type Name = String

data Binding
  = NameBind
  | VarBind Type
  | WireBind Wtype
  deriving (Show, Eq)

type Context = [(Name, Binding)]
type Contexts = (Context, Context) -- (Gamma, Omega)

type Err = String

emptyctx :: Context
emptyctx = []

emptyctxs :: Contexts
emptyctxs = ([], [])

addBinding :: [(Name, b)] -> (Name, b) -> [(Name, b)]
addBinding ctx (x, bind) = (x, bind) : ctx

-- | Add all wire variables in the Pattern to the Context
-- The types of all variables are given in the corresponding position of Wtype

-- | construct an omega from a pattern and its wtype
constructOmega :: MonadError Err m => Pattern -> Wtype -> Context -> m Context
constructOmega pat wt ctx = case pat of
  -- PtVar _ _ -> throwError $ "constructOmega: invalid PtVar in pattern"
  PtEmp -> return ctx
  PtName name -> return $ addBinding ctx (name, WireBind wt)
  PtProd p1 p2 -> case wt of
    WtProd w1 w2 -> do
      ctx' <- constructOmega p1 w1 ctx
      constructOmega p2 w2 ctx'
    _ -> throwError $ "constructOmega: pattern and wtype mismatch"

-- | add pattern wtype bindings
addPatWtypeBinding :: MonadError Err m => Context -> (Pattern, Wtype) -> m Context
addPatWtypeBinding ctx (patvar, wtype) = constructOmega patvar wtype ctx

-- | add pattern type bindings
addPatTypeBinding :: MonadError Err m => Context -> (Pattern, Type) -> m Context
addPatTypeBinding ctx (patvar, ty) = case patvar of
  PtEmp -> return ctx
  PtName name -> return $ addBinding ctx (name, VarBind ty)
  PtProd p1 p2 -> case ty of
    TyProd t1 t2 -> do
      ctx' <- addPatTypeBinding ctx (p1, t1)
      addPatTypeBinding ctx' (p2, t2)
    _ -> throwError $ "addPatTypeBinding: pattern and type mismatch"

-- | add pattern name bindings
addPatNameBinding :: MonadError Err m => Context -> Pattern -> m Context
addPatNameBinding ctx patvar = case patvar of
  -- PtVar _ _    -> throwError "addPatNameBinding: Invalid PtVar in Pattern"
  PtName name  -> return $ addBinding ctx (name, NameBind)
  PtEmp        -> return emptyctx
  PtProd p1 p2 -> do
      ctx' <- addPatNameBinding ctx p1
      addPatNameBinding ctx' p2

name2index :: Context -> Name -> Either Err Int
name2index ctx name =
  case findIndex ((== name). fst) ctx of
    Just ind -> Right ind
    Nothing -> Left $ "Unbounded variable name \"" ++ name ++ "\""

name2entry :: [(Name, b)] -> Name -> Either Err (Name, b)
name2entry ctx name =
  case find ((== name). fst) ctx of
    Just x -> Right x
    Nothing -> Left $ "Unbounded variable name \"" ++ name ++ "\""

index2entry :: [(Name, b)] -> Int -> Either Err (Name, b)
index2entry ctx x
  | length ctx > x = Right $ ctx !! x
  | otherwise = Left $ "getIndex: index " ++ show x
              ++ "is overflow, current length: " ++ show (length ctx)

-- helper functions for interacting with Contexts State
getfst :: MonadState (c1, c2) m => m c1
getfst = fst <$> get
getsnd :: MonadState (c1, c2) m => m c2
getsnd = snd <$> get
setfst :: MonadState (c1, c2) m => c1 -> m ()
setfst ctx = get >>= \(_, omega) -> put (ctx, omega)
setsnd :: MonadState (c1, c2) m => c2 -> m ()
setsnd ctx = get >>= \(gamma, _) -> put (gamma, ctx)

getQwQ :: (MonadState (c, c) m, MonadError Err m) 
       => m c -> (c -> a -> Either Err b) -> a -> m b
getQwQ g f a = do
  ctx <- g
  case f ctx a of
    Right b -> return b
    Left e -> throwError e

-- Gamma uses De Bruijn index
index2entryGamma :: (MonadState Contexts m, MonadError Err m)
            => Int -> m (Name, Binding)
index2entryGamma = getQwQ getfst index2entry

-- Omega uses names
name2entryOmega :: (MonadState Contexts m, MonadError Err m)
            => Name -> m (Name, Binding)
name2entryOmega = getQwQ getsnd name2entry