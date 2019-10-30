module Lexer (
    module Lexer,
    module Parser
) where

import Data.Char
import Parser

spaces :: Parser String
spaces = many (sat isSpace)

digits :: Parser String
digits = many (sat isDigit)

parseStringLiteral :: Parser String
parseStringLiteral = char '"' >> many oneChar << (char '"' <|> oneChar)
    -- `many oneChar` stops when it comes to a '"' or a wrong escape
    -- Thus we may want to try whether there's a '"' or an error raised by escape
    where
        oneChar = do
            ch <- sat (/= '"')  -- not the end of a string
            if ch /= '\\' then return ch else do
                -- escape
                ch2 <- item -- must read
                case ch2 of
                    '"' -> return '"'
                    '\\' -> return '\\'
                    'n' -> return '\n'
                    'r' -> return '\r'
                    a -> throw $ UnexpectedChar 'a'


data Token = EOF
    | WaveAt 
    | SpecialChar Char 
    | StringLiteral String 
    | SemiComma String 
    | NonSpecialChars String 
    deriving(Show, Eq)

eof :: Parser Token
eof = do
    content <- get
    case at content of
        Nothing -> return EOF
        _ -> nonEOFToken >>= throwToken

nonEOFToken :: Parser Token
nonEOFToken = spaces >>
    (string "~@" >> return WaveAt) <|> 
    (SpecialChar <$> sat (`elem` "[]{}()'`~^@")) <|>
    do
        ch  <- pick
        case ch of
            '"' -> StringLiteral <$> parseStringLiteral
            ';' -> next >> SemiComma <$> many (sat (/= '\n'))
            _ -> NonSpecialChars <$> some (sat p)
                where p x = not (isSpace x) && x `notElem` "[]{}()'\"`,;"

tokenWithComment :: Parser Token
tokenWithComment = nonEOFToken <|> eof
                

token :: Parser Token
token = do
    t <- tokenWithComment
    case t of
        SemiComma _ -> token
        a -> return a

peekWithComment :: Parser Token
peekWithComment = dip tokenWithComment

peek :: Parser Token
peek = dip token

throwToken :: Token -> Parser a
throwToken = throw . UnexpectedToken .show

takeToken :: Token -> Parser Token
takeToken k = do
    t <- token
    if t == k then return k else throwToken t

waveAt :: Parser Token
waveAt = takeToken WaveAt

satToken :: (Token -> Maybe a) -> Parser a
satToken p = do
    t <- token
    case p t of
        Nothing -> throwToken t
        Just a -> return a

specialChar :: Parser Char
specialChar = satToken p where
    p (SpecialChar a) = Just a
    p _ = Nothing

stringLiteral :: Parser String
stringLiteral = satToken p where
    p (StringLiteral a) = Just a
    p _ = Nothing

semicomma :: Parser String
semicomma = do
    t <- tokenWithComment
    case t of
        SemiComma a -> return a
        _ -> throwToken t

nonspecialChars :: Parser String
nonspecialChars = satToken p where
    p (NonSpecialChars a) = Just a
    p _ = Nothing


paren :: Parser a -> Parser a
paren a = takeToken (SpecialChar '(') >> a << takeToken (SpecialChar ')')
