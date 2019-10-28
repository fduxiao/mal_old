{-# LANGUAGE LambdaCase #-}
module Eval (
    module Eval,
    module AST,
    module Env
) where

import Control.Monad.State
import Control.Monad
import Control.Applicative
import AST
import Env

defaultEnv :: Env
defaultEnv = [("top", defnFromList [
        ("+", Func "+" $ (MalAtom<$>) . addMany),
        ("-", Func "-" $ (MalAtom<$>) . minusMany),
        ("*", Func "*" $ (MalAtom<$>) . timesMany),
        ("/", Func "/" $ (MalAtom<$>) . divideMany),
        ("throw", Func "throw" $ \case
            [x] -> Left $ MalError x
            _ -> Left . MalError $ MalString "Wrong arguments number")
    ])]

type Reduce = StateT Env (Either (EvalError, Env))

newtype Eval a = Eval {runEval :: Env -> IO (Either EvalError a, Env)}

instance Monad Eval where
    return a = Eval $ \env -> return (Right a, env)
    (Eval ma) >>= f = Eval $ \env -> do
        (result, env') <- ma env
        case result of
            Left e -> return (Left e, env')
            Right a -> runEval (f a) env'

instance MonadIO Eval where
    liftIO io = Eval $ \env -> do
        a <- io
        return (Right a, env)

getEnv :: Eval Env
getEnv = Eval $ \env -> return (Right env, env)

putEnv :: Env -> Eval ()
putEnv env = Eval $ \_ -> return (Right (), env)

instance Applicative Eval where
    pure = return
    f <*> a = f >>= (<$>a)

instance Functor Eval where
    fmap f a = a >>= (return . f)


throw :: EvalError -> Reduce a
throw e = do
    env <- get
    lift $ Left (e, env)

reduce :: Mal -> Reduce Mal
reduce (MalAtom a) = atom a
reduce (Var a) = do
    env <- get
    case find env a of 
        Nothing -> throw $ UndefinedSymbol a
        Just a -> atom a

reduce (MalList []) = atom Nil
reduce (MalList (x:xs)) = do
    f <- eval x
    r <- case f of 
        (Func n a) -> do
            modify $ pushEmptyDefn n
            args <- mapM eval xs
            case a args of
                Right d -> return d
                Left e -> throw e
        _ -> throw $ NotAFunction $ show f
    modify popDefn
    return r

eval :: Mal -> Reduce MalAtom
eval e = do
    d <- reduce e
    case d of
        MalAtom a -> return a
        b -> eval b

runEval2 :: Reduce a -> Env -> Either (EvalError, Env) (a, Env)
runEval2 = runStateT
