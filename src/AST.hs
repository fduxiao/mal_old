module AST (
    module AST,
    module Map,
    liftIO
) where

import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative

type Traceback = String
type CallStack = String

type Defn = Map.Map String MalAtom
data Env = Env {
    callStack :: [CallStack],
    traceback :: [Traceback],
    defn :: [Defn]
}

data EvalError = 
    UndefinedSymbol String 
    | NotAFunction String 
    | MalError MalAtom 
    | InvalidArgNumber 
    | TypeError 
    | ValueError 
    | SyntaxError String deriving(Show)
newtype Eval a = Eval {runEval :: Env -> IO (Either EvalError a, Env)}

instance Monad Eval where
    return a = Eval $ \env -> return (Right a, env)
    (Eval ma) >>= f = Eval $ \env -> do
        (result, env') <- ma env
        case result of
            Left e -> return (Left e, env')
            Right a -> runEval (f a) env'

instance MonadIO Eval where
    liftIO io = Eval $ \env -> do
        a <- io
        return (Right a, env)

instance Applicative Eval where
    pure = return
    f <*> a = f >>= (<$>a)

instance Functor Eval where
    fmap f a = a >>= (return . f)

getEnv :: Eval Env
getEnv = Eval $ \env -> return (Right env, env)

putEnv :: Env -> Eval ()
putEnv env = Eval $ \_ -> return (Right (), env)

modifyEnv :: (Env -> Env) -> Eval ()
modifyEnv f = getEnv >>= (putEnv . f)

throw :: EvalError -> Eval a
throw e = Eval $ \env -> return (Left e, env)

throwAtom :: MalAtom -> Eval a
throwAtom = throw . MalError

joinString :: (Show a) => String -> [a] -> String
joinString _ [] = ""
joinString sep (x:xs) = show x ++ concatMap (\t -> sep ++ show t) xs

showMalString :: (Show a) => [a] -> String
showMalString [] = "()"
showMalString xs = "(" ++ joinString " " xs ++ ")"


data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | AtomList [MalAtom]
    | Boolean Bool
    | Func (Maybe String) ([MalAtom] -> Eval MalAtom)  -- name and its definition
    | Nope

instance Eq MalAtom where
    (Symbol a) == (Symbol b) = a == b
    (MalString a) == (MalString b) = a == b
    (Number a) == (Number b) = a == b
    (Floating a) == (Number b) = a == fromInteger b
    (Number a) == (Floating b) = fromInteger a == b
    (Floating a) == (Floating b) = a == b
    (AtomList a) == (AtomList b) = a == b
    (Boolean a) == (Boolean b) = a == b
    Nope == Nope = True 
    _ == _ = False

instance Show MalAtom where
    show (Symbol s) = ':':s
    show (MalString s) = s
    show (Number i) = show i
    show (Floating f) = show f
    show (AtomList xs) = '\'':showMalString xs
    show (Boolean b) = if b then "true" else "false"
    show (Func Nothing f) = "#<proc>"
    show (Func (Just n) f) = "#<proc:" ++ n ++ ">"
    show Nope = "nope"

symbol :: (Monad m) => String -> m MalAtom
symbol = return . Symbol

malString :: (Monad m) => String -> m MalAtom
malString = return . MalString

number :: (Monad m) => Integer -> m MalAtom
number = return . Number

floating :: (Monad m) => Float -> m MalAtom
floating = return . Floating

bool :: (Monad m) => Bool -> m MalAtom
bool = return . Boolean

list :: (Monad m) => [MalAtom] -> m MalAtom
list = return . AtomList

data Mal = 
    MalAtom MalAtom
    | MalList [Mal] 
    | Var String 
    | Comment String
    | MalIO (IO MalAtom)
    | MalDef String Mal
    | Let [(String, Mal)] Mal
    | Do [Mal]
    | If Mal Mal Mal
    | Fn Mal Mal

instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList xs) = showMalString xs
    show (Comment c) = "; " ++ c
    show (MalDef s mal) = "(def! " ++ s ++ show mal ++ " )"
    show (Do []) = "(Do)"
    show (Do (x:xs)) = "(Do " ++ show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (If a b c) = "(if " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
    show (Fn a b) = "(fn* " ++ show a  ++ " " ++ show b ++ ")"

atom :: (Monad m) => MalAtom -> m Mal
atom = return . MalAtom
