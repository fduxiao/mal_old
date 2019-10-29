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

data EvalError = UndefinedSymbol String | NotAFunction String | MalError MalAtom deriving(Show)
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


data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | Nil
    | Boolean Bool
    | Func String ([MalAtom] -> Eval Mal)  -- name and its definition
    | Nope

instance Show MalAtom where
    show (Symbol s) = ':':s
    show (MalString s) = s
    show (Number i) = show i
    show (Floating f) = show f
    show Nil = "nil"
    show (Boolean b) = if b then "true" else "false"
    show (Func n f) = "proc:" ++ n
    show Nope = ""

data Mal = 
    MalAtom MalAtom
    | MalList [Mal] 
    | Var String 
    | Comment String
    | MalIO (IO MalAtom)
    | Empty

instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList l) = handle l where
        handle [] = "nil"
        handle (x:xs) = '(':show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (Comment c) = ""
    show Empty = ""

atom :: (Monad m) => MalAtom -> m Mal
atom = return . MalAtom

addAtom :: MalAtom -> MalAtom -> Eval MalAtom
addAtom (Number n1) (Number n2) = return $ Number (n1 + n2)
addAtom (Number n1) (Floating n2) = return $ Floating (fromInteger n1 + n2)
addAtom (Floating n1) (Number n2) = return $ Floating (n1 + fromInteger n2)
addAtom (Floating n1) (Floating n2) = return $ Floating (n1 + n2)
addAtom a b = throwAtom . MalString $ "You can not add " ++ show a ++ " and " ++ show b

minusAtom :: MalAtom -> MalAtom -> Eval MalAtom
minusAtom (Number n1) (Number n2) = return $ Number (n1 - n2)
minusAtom (Number n1) (Floating n2) = return $ Floating (fromInteger n1 - n2)
minusAtom (Floating n1) (Number n2) = return $ Floating (n1 - fromInteger n2)
minusAtom (Floating n1) (Floating n2) = return $ Floating (n1 - n2)
minusAtom a b = throwAtom . MalString $ "You can not minus " ++ show a ++ " and " ++ show b

timesAtom :: MalAtom -> MalAtom -> Eval MalAtom
timesAtom (Number n1) (Number n2) = return $ Number (n1 * n2)
timesAtom (Number n1) (Floating n2) = return $ Floating (fromInteger n1 * n2)
timesAtom (Floating n1) (Number n2) = return $ Floating (n1 * fromInteger n2)
timesAtom (Floating n1) (Floating n2) = return $ Floating (n1 * n2)
timesAtom a b = throwAtom . MalString $ "You can not times " ++ show a ++ " and " ++ show b

divideAtom :: MalAtom -> MalAtom -> Eval MalAtom
divideAtom _ (Number 0) = throwAtom $ Number 0
divideAtom _ (Floating 0) = throwAtom $ Floating 0
divideAtom (Number n1) (Number n2)
    | n1 `mod` n2 == 0 = return $ Number (n1 `div` n2)
    | otherwise        = return $ Floating (fromInteger n1 / fromInteger n2)
divideAtom (Floating n1) (Number n2) = return $ Floating (n1 / fromInteger n2)
divideAtom (Number n1) (Floating n2) = return $ Floating (fromInteger n1 /  n2)
divideAtom (Floating n1) (Floating n2) = return $ Floating (n1 / n2)
divideAtom a b = throwAtom . MalString $ "You can not divide " ++ show a ++ " by " ++ show b

putEither :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
putEither f a b = do
    x <- a
    b >>= f x

addMany :: [MalAtom] -> Eval MalAtom
addMany [] = throwAtom $ MalString "Inadequate Operands"
addMany xs = foldl (putEither addAtom) (return $ Number 0) $ fmap return xs

minusMany :: [MalAtom] -> Eval MalAtom
minusMany [] = throwAtom $ MalString "Inadequate Operands"
minusMany [x] = minusAtom (Number 0) x
minusMany (x:xs) = addMany xs >>= minusAtom x

timesMany :: [MalAtom] -> Eval MalAtom
timesMany [] = throwAtom $ MalString "Inadequate Operands"
timesMany xs = foldl (putEither timesAtom) (return $ Number 1) $ fmap return xs

divideMany :: [MalAtom] -> Eval MalAtom
divideMany [] = throwAtom $ MalString "Inadequate Operands"
divideMany [x] = divideAtom (Number 1) x
divideMany (x:xs) = timesMany xs >>= divideAtom x
