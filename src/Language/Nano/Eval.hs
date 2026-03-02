{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude, extendEnv
  , parse, evalOp
  , env0
  )
  where
import Control.Exception (throw, catch)
import Language.Nano.Parser
import Language.Nano.Types hiding (Error) 
import qualified Language.Nano.Types as T (Error(..))
import Data.List (lookup)
import Prelude hiding (lookup)
--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
parse :: String -> Expr
parse = parseExpr
exitError :: T.Error -> IO Value
exitError (T.Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
eval _   (EInt i)         = VInt i
eval _   (EBool b)        = VBool b
eval _   ENil             = VNil
eval env (EVar x)         = lookupId x env
eval env (ELam x e)       = VClos env x e
eval env (EIf c t f)      = case eval env c of
                              VBool True  -> eval env t
                              VBool False -> eval env f
                              _           -> throw (T.Error "type error")
eval env (ELet x e1 e2)   = let env' = extendEnv x v env
                                v    = eval env' e1
                            in eval env' e2
eval env (EBin op e1 e2)  = evalOp op (eval env e1) (eval env e2)
eval env (EApp e1 e2)     = case eval env e1 of
                              VClos cEnv x body -> eval (extendEnv x (eval env e2) cEnv) body
                              VPrim f           -> f (eval env e2)
                              _                 -> throw (T.Error "type error")

--------------------------------------------------------------------------------
-- | `evalOp op v1 v2` returns the result of the binary operation
--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
evalOp Plus  (VInt x)  (VInt y)  = VInt (x + y)
evalOp Minus (VInt x)  (VInt y)  = VInt (x - y)
evalOp Mul   (VInt x)  (VInt y)  = VInt (x * y)
evalOp Lt    (VInt x)  (VInt y)  = VBool (x < y)
evalOp Le    (VInt x)  (VInt y)  = VBool (x <= y)
evalOp Eq v1 v2 = VBool (v1 == v2)
evalOp Ne v1 v2 = VBool (v1 /= v2)
evalOp And   (VBool x) (VBool y) = VBool (x && y)
evalOp Or    (VBool x) (VBool y) = VBool (x || y)
evalOp Cons  v1        v2        = VPair v1 v2
evalOp _     _         _         = throw (T.Error "type error: binop")

--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId x env = case Data.List.lookup x env of
                   Just v  -> v
                   Nothing -> throw (T.Error ("unbound variable: " ++ x))

--------------------------------------------------------------------------------
extendEnv :: Id -> Value -> Env -> Env
--------------------------------------------------------------------------------
extendEnv x v env = (x, v) : env

--------------------------------------------------------------------------------
prelude :: Env
prelude =
  [ ("head", VPrim (\v -> case v of 
                            VPair h _ -> h
                            _         -> throw (T.Error "type error")))
  , ("tail", VPrim (\v -> case v of 
                            VPair _ t -> t
                            _         -> throw (T.Error "type error")))
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]
--------------------------------------------------------------------------------