module Main where

import System.Console.Haskeline
import Control.Monad.Trans
import System.Exit
import System.Environment
import System.IO
import Control.Exception (try)
import Repl

main :: IO ()
main = do
    args <- getArgs
    env <- defaultEnv
    let env' = setArgs args env
    case args of
        [] -> repl env'
        (x:xs) -> do
            result <- try $ readFile x
            case result of
                Left err -> print (err :: IOException) >> exitWith (ExitFailure 3)
                Right content -> runFile content env'

repl :: Env -> IO ()
repl e = runInputT defaultSettings (withInterrupt (loop e))
    where
        loop :: Env -> InputT IO ()
        loop env = handle (\Interrupt -> outputStrLn "User Interruption. Press EOF(^D) to exit." >> loop env) $ do
            line <- getInputLine "user> "
            case line of
                Nothing -> return ()
                Just ":exit" -> return ()
                Just line -> do
                    (p, env') <- liftIO $ repLine line env
                    loop env'
