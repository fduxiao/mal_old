module Eval where

import Data.Map as Map
import Reader
import Control.Monad.State
import AST

type Env = Map.Map String MalAtom

defaultEnv :: Env
defaultEnv = Map.fromList [
        ("+", Func $ MalAtom . addMany),
        ("-", Func $ MalAtom . minusMany),
        ("*", Func $ MalAtom . timesMany),
        ("/", Func $ MalAtom . divideMany)
    ]

type Reduce = StateT Env (Either EvalError)

throw :: EvalError -> Reduce a
throw = lift . Left

reduce :: Mal -> Reduce Mal
reduce (MalAtom a) = atom a
reduce (Var a) = do
    env <- get
    case Map.lookup a env of 
        Nothing -> throw $ UndefinedSymbol a
        Just a -> atom a

reduce (MalList []) = atom Nil
reduce (MalList (x:xs)) = do
    f <- eval x
    case f of 
        (Func a) -> do
            args <- mapM eval xs
            return $ a args
        _ -> throw $ NotAFunction $ show f

eval :: Mal -> Reduce MalAtom
eval e = do
    d <- reduce e
    case d of
        MalAtom a -> return a
        b -> eval b

runEval :: Reduce a -> Env -> Either EvalError (a, Env)
runEval = runStateT
