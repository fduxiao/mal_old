module AST (
    module AST,
    module Map,
    liftIO
) where

import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.IORef as Ref
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
    | EvalError String
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

showMalList :: (Show a) => [a] -> String
showMalList [] = "()"
showMalList xs = "(" ++ joinString " " xs ++ ")"


data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | AtomList [MalAtom]
    | Boolean Bool
    | Func (Maybe String) ([MalAtom] -> Eval MalAtom)  -- name and its definition
    | Macro String ([MalAtom] -> Eval Mal)
    | Nil
    | AtomError MalAtom [Traceback]
    | AtomPtr (Ref.IORef MalAtom)

derefAtom :: MalAtom -> Eval MalAtom
derefAtom (AtomPtr x) = liftIO $ Ref.readIORef x

atomPtr :: MalAtom -> Eval MalAtom
atomPtr x = do
    a <- liftIO $ Ref.newIORef x
    return $ AtomPtr a

setAtomPtr :: MalAtom -> MalAtom -> Eval MalAtom
setAtomPtr (AtomPtr ref) value = liftIO $ Ref.writeIORef ref value >> return value
setAtomPtr _ _ = throw ValueError

showAtom :: MalAtom -> Eval String
showAtom a@(AtomPtr x) = do
    value <- derefAtom a
    ("(atom "++) . (++")") <$> showAtom value
showAtom a  = return $ show a

instance Eq MalAtom where
    (Symbol a) == (Symbol b) = a == b
    (MalString a) == (MalString b) = a == b
    (Number a) == (Number b) = a == b
    (Floating a) == (Number b) = a == fromInteger b
    (Number a) == (Floating b) = fromInteger a == b
    (Floating a) == (Floating b) = a == b
    (AtomList a) == (AtomList b) = a == b
    (Boolean a) == (Boolean b) = a == b
    Nil == Nil = True 
    _ == _ = False

instance Show MalAtom where
    show (Symbol s) = s
    show (MalString s) = show s
    show (Number i) = show i
    show (Floating f) = show f
    show (AtomList xs) = showMalList xs
    show (Boolean b) = if b then "true" else "false"
    show (Func Nothing f) = "#<proc>"
    show (Func (Just n) f) = "#<proc:" ++ n ++ ">"
    show (Macro name _) = "macro!<" ++ name ++ ">"
    show (AtomError error _) = "(throw " ++ show error ++ ")"
    show Nil = "nil"

atomType :: MalAtom -> String
atomType (Symbol _) = "symbol"
atomType (MalString _) = "string"
atomType (Number _) = "number"
atomType (Floating _) = "float"
atomType (AtomList _) = "list"
atomType (Boolean _) = "boolean"
atomType (Func _ _) = "func"
atomType (Macro _ _) = "macro"
atomType Nil = "nil"
atomType (AtomError _ _) = "error"
atomType (AtomPtr _) = "atom"

symbol :: (Monad m) => String -> m MalAtom
symbol = return . Symbol

malString :: (Monad m) => String -> m MalAtom
malString = return . MalString

number :: (Monad m) => Integer -> m MalAtom
number = return . Number

atomList :: (Monad m) => [MalAtom] -> m MalAtom
atomList = return . AtomList

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
    -- | MalIO (IO MalAtom)
    | MalDef String Mal
    | Unset [Mal]
    | Let [(String, Mal)] Mal
    | Do [Mal]
    | If Mal Mal Mal
    | Fn Mal Mal
    | Quote Mal
    | QuasiQuote Mal
    | MacroDef String Mal
    | Try Mal String Mal

instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList xs) = showMalList xs
    show (Comment c) = "; " ++ c
    show (MalDef s mal) = "(def! " ++ s ++ show mal ++ ")"
    show (Unset xs) = "(unset " ++ show xs ++ ")"
    show (Do []) = "(do)"
    show (Do (x:xs)) = "(do " ++ show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (If a b c) = "(if " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
    show (Fn a b) = "(fn* " ++ show a  ++ " " ++ show b ++ ")"
    show (Quote a) = show a
    show (QuasiQuote a) = show a
    show (MacroDef name body) = "(defmacro! " ++ show name ++ " " ++ show body ++ ")"
    show (Try t v c) = "(try* " ++ show t ++ "(catch* " ++ v ++ " " ++ show c ++ "))"


atom :: (Monad m) => MalAtom -> m Mal
atom = return . MalAtom

mal2Atom :: Mal -> MalAtom
mal2Atom (MalAtom s) = s
mal2Atom (MalList xs) = AtomList $ fmap mal2Atom xs
mal2Atom (Var a) = Symbol a
mal2Atom (MalDef name body) = AtomList [Symbol "def!", Symbol name, mal2Atom body]
mal2Atom (Unset names) = AtomList [Symbol "unset!", AtomList $ fmap mal2Atom names]
mal2Atom (Let defns body) = AtomList [Symbol "let*", AtomList $ parseParams defns, mal2Atom body]
    where 
        parseParams [] = []
        parseParams ((name, value):xs) = Symbol name:mal2Atom value:parseParams xs
mal2Atom (Do procs) = AtomList (Symbol "do":fmap mal2Atom procs)
mal2Atom (If c t f) = AtomList [Symbol "if", mal2Atom c, mal2Atom t, mal2Atom f]
mal2Atom (Fn params body) = AtomList [Symbol "fn*", mal2Atom params, mal2Atom body]
mal2Atom (Quote a) = AtomList [Symbol "quote", mal2Atom a]
mal2Atom (QuasiQuote a) = AtomList [Symbol "quasiquote", mal2Atom a]
mal2Atom (MacroDef name macro) = AtomList [Symbol "defmacro!", Symbol name, mal2Atom macro]
mal2Atom (Try t v c) = AtomList [Symbol "try*", mal2Atom t, AtomList [Symbol "catch*", Symbol v, mal2Atom c]]

atom2Mal :: MalAtom -> Maybe Mal
atom2Mal (Symbol x) = return $ Var x
atom2Mal (AtomList [Symbol "def!", Symbol name, body]) = MalDef name <$> atom2Mal body
atom2Mal (AtomList (Symbol "def!":_)) = Nothing
atom2Mal (AtomList [Symbol "unset!", AtomList xs]) = Unset <$> mapM atom2Mal xs
atom2Mal (AtomList (Symbol "unset!":_)) = Nothing
atom2Mal (AtomList [Symbol "let*", AtomList xs, body]) = do
    b <- atom2Mal body
    params <- parseParams xs
    return $ Let params b
    where
        parseParams :: [MalAtom] -> Maybe [(String, Mal)]
        parseParams [] = return []
        parseParams (Symbol name:value:xs) = do
            rest <- parseParams xs
            v <- atom2Mal value
            return $ (name, v):rest
        parseParams _ = Nothing
atom2Mal (AtomList [Symbol "do"]) = Nothing
atom2Mal (AtomList (Symbol "do":xs)) = Do <$> mapM atom2Mal xs 
atom2Mal (AtomList [Symbol "if", c, t, f]) = If <$> atom2Mal c <*> atom2Mal t <*> atom2Mal f
atom2Mal (AtomList (Symbol "if":_)) = Nothing
atom2Mal (AtomList [Symbol "fn*", params, body]) = Fn <$> atom2Mal params <*> atom2Mal body
atom2Mal (AtomList (Symbol "fn*":_)) = Nothing
atom2Mal (AtomList [Symbol "quote", ast]) = Quote <$> atom2Mal ast
atom2Mal (AtomList (Symbol "quote":_)) = Nothing
atom2Mal (AtomList [Symbol "quasiquote", ast]) = QuasiQuote <$> atom2Mal ast
atom2Mal (AtomList (Symbol "quasiquote":_)) = Nothing
atom2Mal (AtomList [Symbol "defmacro!", Symbol name, macro]) = MacroDef name <$> atom2Mal macro
atom2Mal (AtomList (Symbol "defmacro!":_)) = Nothing

atom2Mal (AtomList [Symbol "try*", t, AtomList [Symbol "catch*", Symbol v, c]]) = do
   t' <- atom2Mal t
   c' <- atom2Mal c
   return $ Try t' v c'
atom2Mal (AtomList (Symbol "try*":_)) = Nothing 
atom2Mal (AtomList xs) = MalList <$> mapM atom2Mal xs
atom2Mal a = atom a
