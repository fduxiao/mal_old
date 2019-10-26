module Reader where

import Lexer
import Parser

data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | Nil
    | Boolean Bool
    deriving(Eq)

instance Show MalAtom where
    show (Symbol s) = ':':s
    show (MalString s) = show s
    show (Number i) = show i
    show (Floating f) = show f
    show Nil = "nil"
    show (Boolean b) = if b then "true" else "false"

data Mal = MalAtom MalAtom | MalList [Mal] | Var String | Comment String deriving(Eq)
instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList l) = handle l where
        handle [] = "nil"
        handle (x:xs) = '(':show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (Comment c) = ""

atom :: MalAtom -> Parser Mal
atom = return . MalAtom

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

readMalAtom:: Parser Mal
readMalAtom = do
    t <- token
    case t of
        StringLiteral s -> atom $ MalString s
        NonSpecialChars s ->
            case s of 
                "nil" -> atom Nil
                "true" -> atom $ Boolean True
                (':':a) -> atom $ Symbol a
                a -> return $ Var a
        t -> throwToken t

contents :: Parser a -> Parser a
contents p = p << (zeroOne commentLine >> eof)
