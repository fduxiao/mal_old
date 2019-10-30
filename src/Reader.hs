module Reader (
    module Reader,
    parse, showResult
) where

import Parser
import Lexer
import Control.Monad
import Data.Char
import AST (Mal(..), MalAtom(..), atom)

readForm :: Parser Mal
readForm = do
    t <- peek
    case t of
        SpecialChar '(' -> readMalList
        SemiComma _ -> token >> readForm  -- ignored by readForm
        EOF -> throw UnexpectedEOF
        _ -> readMalAtom

readFormWithEmpty :: Parser Mal
readFormWithEmpty = do
    t <- peek
    case t of
        SpecialChar '(' -> readMalList
        SemiComma _ -> token >> readFormWithEmpty  -- ignored by readForm
        EOF -> return Empty
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
    case ns ++ rest of
        [] -> throw $ Error "EmptyArgs" "You should include at least one argument."
        r -> return r

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
        NonSpecialChars "if" -> token >> If <$> readForm <*> readForm <*> readForm
        NonSpecialChars "fn*" -> token >> Fn <$> readArgs <*> readForm
        _ -> MalList <$> many readForm

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
                "nil" -> atom $ AtomList []
                "true" -> atom $ Boolean True
                "false" -> atom $ Boolean False
                (':':a) -> atom $ Symbol a
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
