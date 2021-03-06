{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State
import Control.Monad.Except
import System.IO

import CDef hiding (Value, Env)
import EvalLazy
import ExprParser
import Data.IORef
import qualified Data.Map as LMap
import qualified Data.Map.Strict as Map
import TypeInf

convertTypeError :: (Functor m, Monad m) => ExceptT TypeError m a -> ExceptT SomeError m a
convertTypeError = mapExceptT $ fmap $ either (Left . SEType) Right

convertEvalError :: (Functor m, Monad m) => ExceptT EvalError m a -> ExceptT SomeError m a
convertEvalError = mapExceptT $ fmap $ either (Left . SEEval) Right

runSt :: (Functor m, Monad m) => St m a -> ExceptT SomeError m a
runSt action = convertTypeError $ (mapExceptT $ \x -> fmap fst (runStateT x 0)) $ action

runEV :: EnvLazy -> EV a -> ExceptT SomeError IO (a, EnvLazy)
runEV env action = convertEvalError $ (mapExceptT $ k) $ action where
    k state' = do
      (res, newenv) <- runStateT state' env
      case res of
        Left x -> return $ Left x
        Right y -> return $ Right (y, newenv)


processExpr :: String -> TypeEnv -> Env -> Expr -> Bool -> ExceptT SomeError IO (TypeScheme, Value)
processExpr !name !tenv !venv !expr !showing = do
  ty <- runSt $ typeInfer tenv expr
  lift $ putStrLn $ name ++ " : " ++ show ty
  (result, _) <- runEV venv $  eval expr 
  (rs, _    ) <- runEV venv $ showValueLazy result
  when showing $ lift $ putStrLn $ " = " ++ rs
  ty `seq` result `seq` return (ty, result)

readCmd :: IO (Either ParseError Command)
readCmd = do
  line <- getLine
  return $ commandOfString line

liftIOToStIO :: IO a -> St IO a
liftIOToStIO x = lift $ lift x


repl :: TypeEnv -> Env -> IO ()
repl !tenv !venv = do
    putStr "> "
    cmdOrErr <- readCmd
    case cmdOrErr of
        Left (ParseError ex) -> putStrLn ex >> repl tenv venv
        Right cmd -> case cmd of
	    CQuit -> return ()
	    _	  -> do
	      tmp <- runExceptT $ processCmd cmd tenv venv
	      case tmp of
	          Left someError      -> do
		      putStrLn $ "error: " ++ show someError
		      repl tenv venv
	          Right (newtenv, newvenv) -> repl newtenv newvenv

processCmd :: Command -> TypeEnv -> Env -> ExceptT SomeError IO (TypeEnv, Env)
processCmd !cmd !tenv !venv =
            case cmd of
                 CLet (Name name) expr -> do
                     (!ty, _) <- processExpr name tenv venv expr True
                     let newtenv = Map.insert name ty     tenv
		     thunk <- lift $ newIORef $ Thunk venv expr
                     let newvenv = LMap.insert name thunk venv
                     return (newtenv, newvenv)
                 CRLets bindings       -> do
                     newtenv <- runSt $ tyRLetBindingsInfer tenv bindings
                     (newvenv, _) <- runEV venv $  getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
                 CExp expr -> processExpr "-" tenv venv expr True >> return (tenv, venv)
                 CQuit     -> error "(>_<)(>_<)"

nextEnv :: Command -> (TypeEnv, Env) -> ExceptT SomeError IO (TypeEnv, Env)
nextEnv !cmd (!tenv, !venv) =
	case cmd of
            CLet (Name name) expr -> do
                     (!ty, _) <- processExpr name tenv venv expr False
                     let newtenv = Map.insert name ty     tenv
		     thunk <- lift $ newIORef $ Thunk venv expr
                     let newvenv = LMap.insert name thunk venv
                     return (newtenv, newvenv)
            CRLets bindings       -> do
                     newtenv <- runSt $ tyRLetBindingsInfer tenv bindings
                     (newvenv, _) <- runEV venv $ getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
            CExp  _   -> return (tenv, venv)
            CQuit     -> return (tenv, venv)

loadFile :: FilePath -> (TypeEnv, Env) -> IO (TypeEnv, Env)
loadFile path (tenv, env) = do
    cont <- readFile path
    case commandsOfString cont of
      Left x -> error $ "Error in loading \"" ++ path ++ "\":\n" ++ show x
      Right cmds -> sub cmds (tenv, env)
        where
	  sub [] (te, ve) = return (te, ve) :: IO (TypeEnv, Env)
	  sub (y : ys) (te, ve) = do
              Right newtve   <- runExceptT $ catchError (nextEnv y (te, ve)) (\_ -> return (te, ve))
              sub ys newtve


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (tenv, venv) <- loadFile "stdlib.txt" (teEmpty, LMap.empty)
  repl tenv venv
  return ()
