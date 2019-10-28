module Reader (
    module Reader,
    module Lexer
) where

import Parser
import Lexer
import Control.Monad
import Data.Char
import AST

readForm :: Parser Mal
readForm = do
    t <- peek
    case t of 
        SpecialChar '(' -> readMalList
        SemiComma _ -> token >> readForm  -- ignored by readForm
        EOF -> throw UnexpectedEOF
        _ -> readMalAtom

commentLine :: Parser Mal
commentLine = Comment <$> semicomma

readMalList :: Parser Mal
readMalList = paren (MalList <$> many readForm)

parseFloat :: Parser Float
parseFloat = do
    n1 <- digits
    dot <- char '.'
    n2 <- digits
    case n1 ++ dot:n2 of
        "." -> throw $ UnexpectedChar '.'
        a -> return $ read a

parseNumberFloat :: String -> Parser Mal
parseNumberFloat s = newContext $ do
    put $ Context 0 s
    r <- (MalAtom . Floating <$> parseFloat) <|> (MalAtom . Number . read <$> digits)
    eof
    return r

readMalAtom :: Parser Mal
readMalAtom = do
    t <- token
    case t of
        StringLiteral s -> atom $ MalString s
        NonSpecialChars s ->
            case s of 
                "nil" -> atom Nil
                "true" -> atom $ Boolean True
                "false" -> atom $ Boolean False
                (':':a) -> atom $ Symbol a
                a -> parseNumberFloat a <|> return (Var a)
        t -> throwToken t

contents :: Parser a -> Parser a
contents p = p << (zeroOne commentLine >> eof)
