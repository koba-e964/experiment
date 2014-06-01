{-# LANGUAGE BangPatterns #-}
module Eval where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

newtype Name = Name String deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VFun Name Env Expr
	   | VRFun Int [(Name, Name, Expr)] Env
           | VCons Value Value
           | VNil
           deriving (Eq)

instance (Show Value) where
  show (VInt  v) = show v
  show (VBool v) = show v
  show (VFun (Name name) _ _) = "fun " ++ name ++ " -> (expr)" 
  show (VRFun ind bindings _) = "vrfun ind=" ++ show ind ++ " -> (expr)"
  show (VCons vcar vcdr) = "[" ++ sub vcar vcdr ++ "]" where
    sub !v VNil = show v
    sub !v1 (VCons v2 v3) = show v1 ++ ", " ++ sub v2 v3
    sub _  _ = error "(>_<) < weird... the last cell of the list is not nil..."
  show VNil = "[]"
data Expr  = EConst Value
           | EVar Name
     	   | EAdd Expr Expr
     	   | ESub Expr Expr
	   | EMul Expr Expr
	   | EDiv Expr Expr
	   | ELt  Expr Expr
           | EEq  Expr Expr
	   | EIf Expr Expr Expr
           | ELet Name Expr Expr
           | ERLets [(Name, Name, Expr)] Expr 
           | EMatch Expr [(Pat, Expr)]
           | EFun Name Expr
           | EApp Expr Expr
           | ECons Expr Expr
           | ENil
           deriving (Eq, Show)
data Pat   = PConst Value
           | PVar   Name
           | PCons  Pat Pat
           | PNil
           deriving (Eq, Show)

data Command
  = CLet    Name Expr 
  | CRLets  [(Name, Name, Expr)]  
  | CExp    Expr 
  | CQuit
  deriving (Eq, Show)



op2Int :: (Int -> Int -> Int) -> Value -> Value -> Value
op2Int f (VInt v1) (VInt v2) = VInt (f v1 v2)
op2Int _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

op2IntBool :: (Int -> Int -> Bool) -> Value -> Value -> Value
op2IntBool f (VInt v1) (VInt v2) = VBool (f v1 v2)
op2IntBool _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

type Env = Map String Value

evalError :: String -> a
evalError str = error $ "Evaluation error:" ++ str

eval :: Env -> Expr -> Value
eval _   (EConst v) = v
eval !env (EVar (Name name)) =
    case Map.lookup name env of
      Just value -> value
      Nothing    -> evalError $ "Unbound variable: " ++ name
eval env (EAdd v1 v2) = op2Int (+) (eval env v1) (eval env v2)
eval env (ESub v1 v2) = op2Int (-) (eval env v1) (eval env v2)
eval env (EMul v1 v2) = op2Int (*) (eval env v1) (eval env v2)
eval env (EDiv v1 v2) = op2Int div (eval env v1) (eval env v2)
eval env (ELt e1 e2)  = op2IntBool (<) (eval env e1) (eval env e2)
eval env (EEq e1 e2)  = op2IntBool (==) (eval env e1) (eval env e2)
eval env (EIf vc v1 v2) =
  case eval env vc of
    VBool b -> if b then (eval env v1) else (eval env v2)
    _	    -> evalError "EIf"
eval env (ELet (Name name) ei eo) =
    let newenv = Map.insert name (eval env ei) env in
      eval newenv eo
eval env (ERLets bindings expr) = eval (getNewEnvInRLets bindings env) expr
eval env (EMatch expr patex) = fromMaybe (evalError "Matching not exhaustive") $ tryMatchAll (eval env expr) env patex
eval env (EFun name expr) = VFun name env expr
eval env (EApp func argv) = evalApp (eval env func) (eval env argv)
eval env (ECons e1 e2) = VCons (eval env e1) (eval env e2)
eval _   ENil          = VNil

evalApp :: Value -> Value -> Value
evalApp fval aval =
  case fval of
    VFun (Name param) fenv expr -> eval (Map.insert param aval fenv) expr
    VRFun ind bindings fenv     ->
      let (_, Name arg, expr) = bindings !! ind
      	  newenv       	      = Map.insert arg aval (getNewEnvInRLets bindings fenv) in
      eval newenv expr
    others                      -> evalError $ "app: not a function: " ++ show others

getNewEnvInRLets :: [(Name, Name, Expr)] -> Env -> Env
getNewEnvInRLets bindings oldenv = sub oldenv bindings 0 where
  sub env [] _ = env
  sub env ((Name fname, _, _) : rest) num =
    sub (Map.insert fname (VRFun num bindings env) env) rest (num + 1)

tryMatchAll :: Value -> Env -> [(Pat, Expr)] -> Maybe Value
tryMatchAll _    _  []                   = Nothing
tryMatchAll val env ((pat, expr) : rest) = case tryMatch val env pat of
  Nothing     -> tryMatchAll val env rest
  Just newenv -> Just (eval newenv expr)

tryMatch :: Value -> Env -> Pat -> Maybe Env
tryMatch !val !env !pat = case pat of
  PConst v          -> if val == v then Just env else Nothing
  PVar (Name vname) -> Just $! Map.insert vname val env
  PCons pcar pcdr   -> case val of
    VCons vcar vcdr -> do
      ex <- tryMatch vcar env pcar
      ey <- tryMatch vcdr env pcdr
      return $! Map.union ey (Map.union ex env)
    _ 	            -> Nothing
  PNil              -> case val of
    VNil  -> Just env
    _     -> Nothing