module Main where

import System.Console.Haskeline
import Control.Monad.Trans

import Repl

main :: IO ()
main = runInputT defaultSettings (withInterrupt (loop defaultEnv))
    where
        loop :: Env -> InputT IO ()
        loop env = handle (\Interrupt -> outputStrLn "User Interruption. Press EOF(^D) to exit." >> loop env) $ do
            line <- getInputLine "user> "
            case line of
                Nothing -> return ()
                Just ":exit" -> return ()
                Just line -> do
                    (p, env') <- liftIO $ rep line env
                    loop env'
