module Main where

import System.Console.Haskeline
import Control.Monad.Trans

import Lib

main :: IO ()
main = runInputT defaultSettings (withInterrupt loop)
    where
        loop :: InputT IO ()
        loop = handle (\Interrupt -> outputStrLn "User Interruption. Press EOF(^D) to exit." >> loop) (do
            line <- getInputLine "user> "
            case line of
                Nothing -> return ()
                Just ":exit" -> return ()
                Just line -> liftIO (rep line) >> loop)
