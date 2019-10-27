module Lib
    ( rep
    ) where

import Reader
import Parser
import Lexer
import Eval
import AST

wrap :: Parser Mal -> String -> String
wrap p s = case parse ((zeroOne commentLine >> eof >> return Nothing) <|> (Just <$> contents p)) s of
    (Right Nothing, _) -> ""
    r@(Left _, _) -> showResult r  ++ "\n"
    (Right (Just a), c) -> case runEval (eval a) defaultEnv of
        Left e -> show e ++ "\n"
        Right (r, _) -> show r ++ "\n"

    

rep :: String -> IO ()
rep s = putStr $ wrap readForm s
