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
-- data Name
--   = Var String
--   | Wir Pattern
--   deriving (Show, Eq)
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

-- setQwQ :: MonadState c m => (c -> a -> c) -> (a -> m ())
-- setQwQ f a = do
--   ctx <- get
--   put $ f ctx a

-- setQwQParser :: Monad m => (c -> a -> c) -> (a -> ParsecT s c m ())
-- setQwQParser f a = do
--   ctx <- getState
--   setState $ f ctx a

addBinding :: Context -> (Name, Binding) -> Context
addBinding ctx (x, bind) = (x, bind) : ctx

-- addBindingM :: MonadState Context m => (Name, Binding) -> m ()
-- addBindingM = setQwQ addBinding

-- addBindingP :: (Name, Binding) -> Parser ()
-- addBindingP = setQwQParser addBinding

-- | Add all wire variables in the Pattern to the Context
-- The types of all variables are given in the corresponding position of Wtype

addPatWtypeBinding :: MonadError Err m => Context -> (Pattern, Wtype) -> m Context
addPatWtypeBinding ctx (patvar, wtype) = case patvar of
  PtVar _ _    -> throwError "Invalid PtVar in Pattern"
  PtName name  -> return $ addBinding ctx (name, WireBind wtype)
  PtEmp        -> return emptyctx
  PtProd p1 p2 -> case wtype of
    WtProd w1 w2 -> do
      ctx' <- addPatWtypeBinding ctx (p1, w1)
      addPatWtypeBinding ctx' (p2, w2)
    _            -> throwError "Pattern and Wtype mismatch"

addPatNameBinding :: MonadError Err m => Context -> Pattern -> m Context
addPatNameBinding ctx patvar = case patvar of
  PtVar _ _    -> throwError "Invalid PtVar in Pattern"
  PtName name  -> return $ addBinding ctx (name, NameBind)
  PtEmp        -> return emptyctx
  PtProd p1 p2 -> do
      ctx' <- addPatNameBinding ctx p1
      addPatNameBinding ctx' p2

-- addPatWtypeBindingP :: (Pattern, Wtype) -> Parser ()
-- addPatWtypeBindingP = setQwQParser addPatWtypeBinding

-- addPatNameBindingP :: Pattern -> Parser ()
-- addPatNameBindingP = setQwQParser addPatNameBinding

-- addPatWtypeBindingM :: MonadState Context m => (Pattern, Wtype) -> m ()
-- addPatWtypeBindingM = setQwQ addPatWtypeBinding

-- addPatNameBindingM :: MonadState Context m => Pattern -> m ()
-- addPatNameBindingM = setQwQ addPatNameBinding

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

name2index :: Context -> Name -> Either Err Int
name2index ctx name =
  case findIndex ((== name). fst) ctx of
    Just ind -> Right ind
    Nothing -> Left $ "Unbounded variable name \"" ++ name ++ "\""

name2entry :: Context -> Name -> Either Err (Name, Binding)
name2entry ctx name =
  case find ((== name). fst) ctx of
    Just x -> Right x
    Nothing -> Left $ "Unbounded variable name \"" ++ name ++ "\""

index2entry :: Context -> Int -> Either Err (Name, Binding)
index2entry ctx x
  | length ctx > x = Right $ ctx !! x
  | otherwise = Left $ "getIndex: index " ++ show x
              ++ "is overflow, current length: " ++ show (length ctx)

index2bind :: Context -> Int -> Either Err Binding
index2bind ctx x = fmap snd $ index2entry ctx x

getfst :: (MonadState (c1, c2) m, MonadError Err m) => m c1
getfst = fst <$> get
getsnd :: (MonadState (c1, c2) m, MonadError Err m) => m c2
getsnd = snd <$> get
setfst :: (MonadState (c1, c2) m, MonadError Err m) => c1 -> m ()
setfst ctx = get >>= \(_, omega) -> put (ctx, omega)
setsnd :: (MonadState (c1, c2) m, MonadError Err m) => c2 -> m ()
setsnd ctx = get >>= \(gamma, _) -> put (gamma, ctx)

getQwQ :: (MonadState (c, c) m, MonadError Err m) 
       => m c -> (c -> a -> Either Err b) -> a -> m b
getQwQ g f a = do
  ctx <- g
  case f ctx a of
    Right b -> return b
    Left e -> throwError e

index2Gamma :: (MonadState Contexts m, MonadError Err m)
            => Int -> m (Name, Binding)
index2Gamma = getQwQ getfst index2entry

index2Omega :: (MonadState Contexts m, MonadError Err m)
            => Int -> m (Name, Binding)
index2Omega = getQwQ getsnd index2entry

name2Omega :: (MonadState Contexts m, MonadError Err m)
            => Name -> m (Name, Binding)
name2Omega = getQwQ getsnd name2entry