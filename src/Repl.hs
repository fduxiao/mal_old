module Repl (
    rep,
    runRepl,
    defaultEnv,
    Env
) where

import Reader
import Eval
import Core
import Data.Map as Map
import Control.Monad.State

type Repl a = StateT Env IO a

runRepl :: Repl a -> Env -> IO (a, Env)
runRepl = runStateT


evalToRepl :: Eval MalAtom -> Repl ()
evalToRepl (Eval e) = do
    env <- get
    (r, env') <- lift $ e env
    put env'
    case r of
        Left err -> printEvalError err
        Right Nope -> return ()
        Right a -> lift $ print a

printEvalError :: EvalError -> Repl ()
printEvalError err = do
    env <- get
    printStack (traceback env) >> modify clearTraceback >> liftIO (print err)
    where
        printStack [] = return ()
        printStack xxs = sequence_ $ do
            x <- reverse xxs
            return $ liftIO (putStrLn x)


rep :: String -> Env -> IO ((), Env)
rep s env = flip runRepl env $ case parse (contents tops) s of
    r@(Left _, _) -> liftIO . putStrLn $ showResult r
    (Right a, _) -> sequence_ $ evalToRepl <$> fmap eval a
