module Lib
    ( rep
    ) where

import Reader
import Eval
import Data.Map as Map
import Control.Monad.State

wrap :: Parser Mal -> Parser (Maybe Mal)
wrap p = (zeroOne commentLine >> eof >> return Nothing) <|> (Just <$> contents p)

printEvalError :: (EvalError, Env) -> IO ()
printEvalError (err, env) = printCallStack env >> print err
    where
        printCallStack [] = return ()
        printCallStack xxs = sequence_ $ do
            x <- reverse xxs
            return $ print x


rep :: String -> IO ()
rep s = case parse (wrap readForm) s of -- parse
    r@(Left _, _) -> putStrLn $ showResult r
    (Right Nothing, _) -> putStr ""
    (Right (Just a), _) -> -- eval
        case runEval2 (eval a) defaultEnv of
            Right (r, _) -> print r
            Left e -> printEvalError e

