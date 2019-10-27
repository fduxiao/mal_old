module Parser (
    module Parser,
    module Control.Applicative,
    void
) where

import Control.Monad
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Title = String

type Desc = String

data ParsingError = 
    UnexpectedEOF 
    | UnexpectedChar Char 
    | UnexpectedToken String 
    | Error Title Desc 
    | Zero 
    | UnendedString deriving(Eq)

instance Show ParsingError where
    show UnexpectedEOF = "Unexpected EOF"
    show (UnexpectedChar ch) = "Unexpected Char: " ++ [ch]
    show (UnexpectedToken s) = "Unexpected Token: " ++ s
    show (Error t d) = t ++ ": " ++ d
    show UnendedString = "UnendedString"

data Context = Context {
    pos :: Int,
    text :: String
}

nextContext :: Context -> Context
nextContext c = Context (pos c + 1) $ text c

at :: Context -> Maybe Char
at (Context p t) = if length t <= p then Nothing else Just $ t !! p

instance Show Context where
    show context = show (pos context) ++  (':':text context)


type ParsingResult a = (Either ParsingError a, Context)

showResult :: (Show a) => ParsingResult a -> String
showResult (Left e, context) = show context ++ " : " ++ show e
showResult (Right v, context) = show v

newtype Parser a =  Parser {runParser :: Context -> ParsingResult a}

parse :: Parser a -> String -> ParsingResult a
parse p s = runParser p (Context 0 s)

instance Monad Parser where
    return a = Parser $ (,) $ Right a
    a >>= f = Parser $ \context ->
        let (r, c) = runParser a context in
            case r of
                Right a' -> runParser (f a') c
                Left err -> (Left err, c)

instance Applicative Parser where
    pure = return
    f <*> a = f >>= (<$>a)

instance Functor Parser where
    fmap f a = a >>= \a' -> return $ f a'


instance Alternative Parser where
    empty = mzero 
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = pick >>= throw . UnexpectedChar  -- throw some error
    m1 `mplus` m2 = Parser $ \context ->
        let (r, c) = runParser m1 context in
            case r of
                Right a -> (Right a, c) -- passing the state
                Left e -> runParser m2 context  -- given the old state


get :: Parser Context
get = Parser $ \s -> (Right s,s)

put :: Context -> Parser ()
put s = Parser $ const (Right (), s)

throw :: ParsingError -> Parser a
throw err = Parser $ (,) $ Left err

except :: (ParsingError -> Parser a) -> Parser a -> Parser a
except except code = Parser $ \context ->
    case runParser code context of
        (Left e, context') -> runParser (except e) context'
        a -> a

try :: Parser a -> Parser a
try p = do
    context <- get
    flip except p $ \err -> do
        put context  -- maintain old state
        throw err

dip :: Parser a -> Parser a
dip p = do
    context <- get
    a <- p
    put context
    return a

fail :: Parser a -> (a -> Parser ParsingError) -> Parser ParsingError
fail p s = Parser $ \context ->
    case runParser p context of
        (Right a, context') -> runParser (s a) context'
        (Left e, context') -> (Right e, context')

pick :: Parser Char
pick = do
    context <- get 
    case at context of
        Nothing -> throw UnexpectedEOF
        Just ch -> return ch


next :: Parser ()
next = do
    context <- get
    put $ nextContext context

item :: Parser Char
item  = pick << next

sat :: (Char -> Bool) -> Parser Char
sat p = do
    ch <- pick
    if p ch then
        next >> return ch 
        else throw $ UnexpectedChar ch

nsat :: (Char -> Bool) -> Parser Char
nsat p = sat (not . p)


char :: Char -> Parser Char
char ch = sat (== ch)

string :: String -> Parser String
string = mapM char

zeroOne :: Parser a -> Parser [a]
zeroOne p = ((:[]) <$> p) <|> return []

(<<) :: Monad m => m a -> m b -> m a
a << b = do
    s <- a
    b
    return s

sep :: Parser a -> Parser b -> Parser [a]
sep a s = sep1 a s <|> return []

sep1 :: Parser a -> Parser b -> Parser [a]
sep1 a s = (:) <$> a <*> ((s >> sep a s) <|> return [])

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) mzero

newContext :: Parser a -> Parser a
newContext p = do
    context <- get
    r <- p
    put context
    return r
