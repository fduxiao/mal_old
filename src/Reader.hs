module Reader (
    module Reader,
    parse, showResult
) where

import Parser
import Lexer
import Control.Monad
import Data.Char
import AST (Mal(..), MalAtom(..), atom)

readPlainForm :: Parser Mal
readPlainForm = do
    t <- peek
    case t of
        SpecialChar '(' -> readPlainMalList
        SpecialChar '[' -> readPlainMalList
        SemiComma _ -> token >> readPlainForm  -- ignored by readForm
        EOF -> throw UnexpectedEOF
        SpecialChar '@' -> token >> do
            a <- readPlainForm
            return $ MalList [Var "deref", a]
        SpecialChar '\'' -> token >> Quote <$> readPlainForm
        SpecialChar '`' -> token >> QuasiQuote <$> readPlainForm
        SpecialChar '~' -> token >> do
            content <- readPlainForm
            return $ MalList [Var "unquote", content]
        WaveAt -> token >> do
            content <- readPlainForm
            return $ MalList [Var "splice-unquote", content]
        _ -> readMalAtom

readPlainMalList :: Parser Mal
readPlainMalList = MalList <$> paren (many readPlainForm)

readForm :: Parser Mal
readForm = do
    t <- peek
    case t of
        SpecialChar '(' -> readMalList
        SpecialChar '[' -> readMalList
        SemiComma _ -> token >> readForm  -- ignored by readForm
        EOF -> throw UnexpectedEOF
        SpecialChar '@' -> token >> do
            a <- readForm
            return $ MalList [Var "deref", a]
        SpecialChar '\'' -> token >> Quote <$> readPlainForm
        SpecialChar '`' -> token >> QuasiQuote <$> readPlainForm
        SpecialChar '~' -> token >> do
            content <- readForm
            return $ MalList [Var "unquote", content]
        WaveAt -> token >> do
            content <- readForm
            return $ MalList [Var "splice-unquote", content]
        _ -> readMalAtom

commentLine :: Parser Mal
commentLine = Comment <$> semicomma

parseLetArgs :: [Mal] -> Parser [(String, Mal)]
parseLetArgs [] = return []
parseLetArgs (Var name:value:rest) = ((name, value):) <$> parseLetArgs rest
parseLetArgs _ = throw $ Error "LetError" "Wrong binging pairs"

parseArbitraryArg :: Parser [Mal]
parseArbitraryArg = do
    name <- varName
    if name /= "&" then throw $ Error "UnexpectedToken" name
    else do
        name2 <- varName
        if name2 == "&" then throw $ Error "UnexpectedToken" name2 else return [Var name, Var name2]

normalVar :: Parser Mal
normalVar = do
    name <- varName
    if name == "&" then throw $ Error "UnexpectedToken" name else return (Var name)

manyArgs :: Parser [Mal]
manyArgs = do
    ns <- many normalVar
    rest <- parseArbitraryArg <|> return []
    return $ ns ++ rest

readArgs :: Parser Mal
readArgs = var <|> paren (MalList <$> manyArgs)


readMalList :: Parser Mal
readMalList = paren $ do
    t <- peek
    case t of
        NonSpecialChars "def!" -> do
            token
            args <- readArgs
            case args of
                Var name -> MalDef name <$> readForm
                (MalList (Var name:params)) -> MalDef name . Fn (MalList params) <$> readForm
                form -> throw $ Error "InvalidParamForm" (show form)
        NonSpecialChars "let*" -> do
            token
            params <- paren $ many readForm
            defs <- parseLetArgs params
            Let defs <$> readForm
        NonSpecialChars "do" -> token >> Do <$> some readForm
        NonSpecialChars "if" -> token >> If <$> readForm <*> readForm <*> (readForm <|> atom Nil)
        NonSpecialChars "fn*" -> token >> Fn <$> readArgs <*> readForm
        NonSpecialChars "unset!" -> token >> Unset <$> some var
        NonSpecialChars "quote" -> token >> Quote <$> readForm
        NonSpecialChars "quasiquote" -> token >> QuasiQuote <$> readForm
        NonSpecialChars "defmacro!" -> do
            token
            args <- readArgs
            case args of
                Var name -> MacroDef name <$> readForm
                (MalList (Var name:params)) -> MacroDef name . Fn (MalList params) <$> readForm
                form -> throw $ Error "InvalidParamForm" (show form)
        NonSpecialChars "try*" -> do
            token
            t <- readForm
            (var, c) <- paren $ takeToken (NonSpecialChars "catch*") >> ((,) <$> varName <*> readForm)
            return $ Try t var c
        _ -> MalList <$> many readForm

parseNumberFloat :: String -> Parser Mal
parseNumberFloat s = newContext $ do
    put $ Context 0 s
    sign <- zeroOne $ char '-'
    int <- digits
    dot <- zeroOne $ char '.'
    r <- if dot == "" then
            case sign ++ int of
                "" -> throw $ Error "NotANumber" ""
                "-" -> throw $ Error "NotANumber" ""
                a -> atom . Number .read $ a
        else do -- must have a dot
            decimals <- digits
            case sign ++ int ++ dot ++ decimals of
                "." -> throw $ Error "NotANumber" ""
                "-." -> throw $ Error "NotANumber" ""
                ('-':'.':xs) -> atom . Floating . read $ ('-':'0':'.':xs)
                a -> atom . Floating . read $ case decimals of
                    [] -> a ++ "0"
                    _ -> a
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
                -- (':':a) -> atom $ Symbol a -- this is a wrong implemenetation
                a -> parseNumberFloat a <|> return (Var a)
        t -> throwToken t

varName :: Parser String
varName = do
    p <- readMalAtom
    case p of
        Var a -> return a
        _ -> throw $ Error "InvalidPattern" "Not a variable"

var :: Parser Mal
var = Var <$> varName

contents :: Parser a -> Parser a
contents p = p << (zeroOne commentLine >> eof)

tops  :: Parser [Mal]
tops = do
    forms <- many readForm
    case forms of
        [] -> do
            t <- peek
            case t of
                EOF -> return []
                SemiComma _ -> token >> tops
                _ -> some readForm -- trigger error
        _ -> return forms
