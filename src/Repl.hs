{-# LANGUAGE LambdaCase #-}
module Repl (
    repLine,
    runRepl,
    defaultEnv,
    runFile,
    setArgs,
    Env
) where

import Reader
import Eval
import Env
import Core
import Data.Map as Map
import Control.Monad.State
import System.Exit

type Repl a = StateT Env IO a

runRepl :: Repl a -> Env -> IO (a, Env)
runRepl = runStateT

wrapNil :: MalAtom -> Eval (Maybe String)
wrapNil Nil = return Nothing
wrapNil a = Just <$> showAtom a


evalToRepl :: Eval MalAtom -> Repl ()
evalToRepl e = do
    env <- get
    (r, env') <- lift $ runEval (e >>= wrapNil) env
    put env'
    case r of
        Left err -> printEvalError err
        Right Nothing -> return ()
        Right (Just a) -> lift $ putStrLn a


printTraceback :: [Traceback] -> IO ()
printTraceback [] = return ()
printTraceback xs = sequence_ $ do
    x <- reverse xs
    return $ putStrLn x

printEvalError :: EvalError -> Repl ()
printEvalError err = do
    env <- get
    liftIO (printTraceback $ traceback env) >> modify clearTraceback >> liftIO (print err)

repLine :: String -> Env -> IO ((), Env)
repLine s env = flip runRepl env $ case parse (contents tops) s of
    r@(Left _, _) -> liftIO . putStrLn $ showResult r
    (Right a, _) -> sequence_ $ evalToRepl <$> fmap (wrapEval . eval) a

setArgs :: [String] -> Env -> Env
setArgs argv = setDefn "*ARGV*" (AtomList $ fmap MalString argv)

wrapEval :: Eval a -> Eval a
wrapEval e = catch e $ \case
        MalError (AtomError value tb) -> do
            env <- getEnv
            putEnv env{traceback=traceback env ++ tb}
            throw $ MalError value
        err -> throw err

runFile :: String -> Env -> IO ()
runFile lines env = case parse (contents tops) lines of
        r@(Left _, _) -> putStrLn (showResult r) >> exitWith (ExitFailure 1)
        (Right a, _) ->  do
            (value, env') <- runEval (last <$> mapM (wrapEval . eval) a >>= wrapNil) env
            case value of
                Left err -> printTraceback (traceback env') >> print err >> exitWith (ExitFailure 2)
                Right Nothing -> return ()
                Right (Just a) -> putStrLn a
