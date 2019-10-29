{-# LANGUAGE LambdaCase #-}
module Eval (
    module Eval,
    module AST,
    module Env
) where

import AST
import Env

defaultEnv :: Env
defaultEnv = Env {
    defn = [defnFromList [
        ("+", Func "+" $ (MalAtom<$>) . addMany),
        ("-", Func "-" $ (MalAtom<$>) . minusMany),
        ("*", Func "*" $ (MalAtom<$>) . timesMany),
        ("/", Func "/" $ (MalAtom<$>) . divideMany),
        ("throw", Func "throw" $ \case
            [x] -> throwAtom x
            _ -> throwAtom $ MalString "Wrong arguments number"),
        ("symbols", Func "symbols" $ const printSymbol),
        ("print", Func "print" malPrint)
    ]],
    callStack = [],
    traceback = []
}

malPrint :: [MalAtom] -> Eval Mal
malPrint xs = liftIO $ mapM_ print xs >> atom Nope

printSymbol :: Eval Mal
printSymbol = do
    env <- getEnv
    liftIO . print $ mergeDefn env
    atom Nope

catch :: Eval a -> (EvalError -> Eval a) -> Eval a
catch (Eval ea) handle = Eval $ \env -> do
    (a, env') <- ea env
    case a of
        Left e -> runEval (handle e) env'
        Right a -> return (Right a, env')

ensureCallStack :: CallStack -> Eval a -> Eval a
ensureCallStack stack (Eval p) = Eval $ \env -> do
    (a, env) <- p (pushCallStack stack env)
    return (a, popCallStack env)

reduce :: Mal -> Eval Mal
reduce (MalAtom a) = atom a
reduce (Var a) = do
    env <- getEnv
    case find env a of 
        Nothing -> throw $ UndefinedSymbol a
        Just a -> atom a

reduce Empty = atom Nope
reduce (MalList []) = atom Nil
reduce (MalList (x:xs)) = do
    f <- eval x
    r <- case f of 
        (Func n a) -> ensureCallStack n $ do
            modifyEnv $ pushTraceback n
            args <- mapM eval xs
            a args
        _ -> throw $ NotAFunction $ show f
    modifyEnv popTraceback
    return r

eval :: Mal -> Eval MalAtom
eval e = do
    d <- reduce e
    case d of
        MalAtom a -> return a
        b -> eval b
