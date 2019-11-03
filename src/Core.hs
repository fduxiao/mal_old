{-# LANGUAGE LambdaCase #-}
module Core (
    module Core
) where

import AST
import Env
import Eval
import Reader
import Control.Monad
import Control.Exception (try, IOException)

import qualified Data.Map as Map
import System.IO (readFile)
import System.Exit

-- global defines

-- arithmetic
addAtom :: MalAtom -> MalAtom -> Eval MalAtom
addAtom (Number n1) (Number n2) = number (n1 + n2)
addAtom (Number n1) (Floating n2) = floating (fromInteger n1 + n2)
addAtom (Floating n1) (Number n2) = floating (n1 + fromInteger n2)
addAtom (Floating n1) (Floating n2) = floating (n1 + n2)
addAtom a b = throwStringErr $ "You can not add " ++ show a ++ " and " ++ show b

minusAtom :: MalAtom -> MalAtom -> Eval MalAtom
minusAtom (Number n1) (Number n2) = number (n1 - n2)
minusAtom (Number n1) (Floating n2) = floating (fromInteger n1 - n2)
minusAtom (Floating n1) (Number n2) = floating (n1 - fromInteger n2)
minusAtom (Floating n1) (Floating n2) = floating (n1 - n2)
minusAtom a b = throwStringErr $ "You can not minus " ++ show a ++ " and " ++ show b

timesAtom :: MalAtom -> MalAtom -> Eval MalAtom
timesAtom (Number n1) (Number n2) = number (n1 * n2)
timesAtom (Number n1) (Floating n2) = floating (fromInteger n1 * n2)
timesAtom (Floating n1) (Number n2) = floating (n1 * fromInteger n2)
timesAtom (Floating n1) (Floating n2) = floating (n1 * n2)
timesAtom a b = throwStringErr $ "You can not times " ++ show a ++ " and " ++ show b

divideAtom :: MalAtom -> MalAtom -> Eval MalAtom
divideAtom _ (Number 0) = throw $ Number 0
divideAtom _ (Floating 0) = throw $ Floating 0
divideAtom (Number n1) (Number n2)
    | n1 `mod` n2 == 0 = number (n1 `div` n2)
    | otherwise        = floating (fromInteger n1 / fromInteger n2)
divideAtom (Floating n1) (Number n2) = floating (n1 / fromInteger n2)
divideAtom (Number n1) (Floating n2) = floating (fromInteger n1 /  n2)
divideAtom (Floating n1) (Floating n2) = floating (n1 / n2)
divideAtom a b = throwStringErr $ "You can not divide " ++ show a ++ " by " ++ show b

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = join (f <$> a <*> b)

addMany :: [MalAtom] -> Eval MalAtom
addMany [] = throwStringErr "Inadequate Operands"
addMany xs = foldl (bind2 addAtom) (number 0) $ fmap return xs

minusMany :: [MalAtom] -> Eval MalAtom
minusMany [] = throwStringErr "Inadequate Operands"
minusMany [x] = minusAtom (Number 0) x
minusMany (x:xs) = addMany xs >>= minusAtom x

timesMany :: [MalAtom] -> Eval MalAtom
timesMany [] = throwStringErr "Inadequate Operands"
timesMany xs = foldl (bind2 timesAtom) (number 1) $ fmap return xs

divideMany :: [MalAtom] -> Eval MalAtom
divideMany [] = throwStringErr "Inadequate Operands"
divideMany [x] = divideAtom (Number 1) x
divideMany (x:xs) = timesMany xs >>= divideAtom x

eq :: MalAtom -> MalAtom -> Eval Bool
eq (Func _ _) _ = throwStringErr "ValueError"
eq _ (Func _ _) = throwStringErr "ValueError"
eq a b = return $ a == b

lessThan :: MalAtom -> MalAtom -> Eval Bool
lessThan (Number a) (Number b) = return $ a < b
lessThan (Number a) (Floating b) = return $ fromInteger a < b
lessThan (Floating a) (Number b) = return $ a < fromInteger b
lessThan (Floating a) (Floating b) = return $ a < b
lessThan (MalString a) (MalString b) = return $ a < b
lessThan (AtomList _) (AtomList []) = return False 
lessThan (AtomList []) (AtomList _) = return True
lessThan (AtomList (a:as)) (AtomList (b:bs)) = do
    first <- eq a b
    if first then lessThan (AtomList as) (AtomList bs) else a `lessThan` b
lessThan _ _ = throwStringErr "ValueError"

lessThanEq :: MalAtom -> MalAtom -> Eval Bool
lessThanEq a b = (||) <$> eq a b <*> lessThan a b

greaterThan :: MalAtom -> MalAtom -> Eval Bool
greaterThan a b = not <$> lessThanEq a b

greaterThanEq :: MalAtom -> MalAtom -> Eval Bool
greaterThanEq a b = not <$> lessThan a b

compareMany :: (MalAtom -> MalAtom -> Eval Bool) -> [MalAtom] -> Eval MalAtom
compareMany _ [] = throwStringErr "InvalidArgNumber"
compareMany _ [x] = bool True
compareMany p (a:b:xs) = do
    r <- p a b
    if r then compareMany p (b:xs) else bool False

symbols :: Eval MalAtom
symbols = do
    env <- getEnv
    let keys = Map.keys $ mergeDefn env
    return . AtomList $ fmap MalString keys

func :: String -> ([MalAtom] -> Eval MalAtom) -> (String, MalAtom)
func name body = (name, Func (Just name) body)

evil :: String -> Eval MalAtom
evil s = case parse (contents tops) s of 
        r@(Left _, _) -> throwStringErr $ "SyntaxError" ++ showResult r
        (Right a, _) -> eval $ Do a

swapAtom :: [MalAtom] -> Eval MalAtom
swapAtom (x@(AtomPtr _):xs) = do
    value <- derefAtom x
    v <- compose xs [value]
    setAtomPtr x v
    where
        compose :: [MalAtom] -> [MalAtom] -> Eval MalAtom
        compose [Func _ func] = func
        compose (Func _ func:xs) = \rest -> do
            first <- func rest
            compose xs [first]
        compose _ = const $ throwStringErr "ValueError"

swapAtom [_] = throwStringErr "InvalidArgNumber"
swapAtom _ = throwStringErr "ValueError"


prStr :: [MalAtom] -> String
prStr [] = ""
prStr [x] = show x
prStr (x:xs) = show x ++ ' ':prStr xs

str :: [MalAtom] -> String
str [] = ""
str [MalString s] = s
str [x] = show x
str (MalString s:xs) = s ++ str xs
str (x:xs) = show x ++ str xs

printLn :: [MalAtom] -> IO ()
printLn [] = putStrLn ""
printLn [MalString s] = putStrLn s
printLn [x] = print x
printLn (MalString s:xs) = putStr s >> putChar ' ' >> printLn xs
printLn (x:xs) = putStr (show x) >> putChar ' ' >> printLn xs


concatList :: [MalAtom] -> Eval MalAtom
concatList [] = atomList []
concatList [AtomList xs] = atomList xs
concatList (AtomList x:rest) = do
    r <- concatList rest
    case r of
        AtomList xs -> atomList $ x ++ xs
        _ -> throwStringErr "ValueError"
concatList _ = throwStringErr "ValueError"

-- global define
defaultDefn :: Defn
defaultDefn = defnFromList [
        func "+" addMany,
        func "-" minusMany,
        func "*" timesMany,
        func "/" divideMany,
        func "throw" $ \case
            [x] -> throw x
            _ -> throwStringErr "Wrong arguments number",
        func "symbols" $ const symbols,
        func "pr-str" $ \xs -> malString $ prStr xs,
        func "str" $ \xs -> malString $ str xs,
        func "prn" $ \xs -> liftIO (putStrLn $ prStr xs) >> return Nil,
        func "println" $ \xs -> liftIO (printLn xs) >> return Nil,
        func "put-str" $ \xs -> liftIO (putStr $ str xs) >> return Nil,
        func "list" list,
        func "cons" $ \case
            [a, AtomList xs] -> list $ a:xs
            [a, _] -> throwStringErr "TypeError"
            _ -> throwStringErr "InvalidArgNumber",
        func "car" $ \case
            [AtomList (x:_)] -> return x
            _ -> throwStringErr "TypeError",
        func "cdr" $ \case
            [AtomList (_:xs)] -> list xs
            _ -> throwStringErr "TypeError",
        func "list?" $ \case
            [AtomList _] -> bool True
            [_] -> bool False 
            _ -> throwStringErr "InvalidArgNumber",
        func "count" $ \case
            [AtomList xs] -> number . sum . fmap (const 1) $ xs
            _ -> throwStringErr "TypeError",
        func "empty?" $ \case
            [AtomList []] -> bool True
            [_] -> bool False 
            _ -> throwStringErr "InvalidArgNumber",
        func "nil?" $ \case
            [Nil] -> bool True
            [_] -> bool False
            _ -> throwStringErr "InvalidArgNumber",
        func "=" $ compareMany eq,
        func "<" $ compareMany lessThan,
        func "<=" $ compareMany lessThanEq,
        func ">" $ compareMany greaterThan,
        func ">=" $ compareMany greaterThanEq,
        func "evil" $ \case
            [MalString s] -> evil s
            _ -> throwStringErr "ValueError",
        func "atom" $ \case
            [x] -> atomPtr x
            _ -> throwStringErr "InvalidArgNumber",
        func "atom?" $ \case
            [AtomPtr _] -> bool True
            [_] -> bool False
            _ -> throwStringErr "InvalidArgNumber",
        func "deref" $ \case
            [AtomPtr a] -> derefAtom (AtomPtr a)
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "reset!" $ \case
            [a, b] -> setAtomPtr a b
            _ -> throwStringErr "InvalidArgNumber",
        func "swap!" swapAtom,
        func "nope" . const $ return Nil,
        func "mal-string-many" $ \case
            [MalString input] -> case parse (contents tops) input of
                r@(Left _, _) -> throwStringErr $ showResult r
                (Right a, _) -> case mal2Atom <$> a of
                    [] -> throwStringErr "no input"
                    xs -> atomList xs
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "read-string-many" $ \case
            [MalString input] -> case parse (contents plaintops) input of
                r@(Left _, _) -> throwStringErr $ showResult r
                (Right a, _) -> case mal2Atom <$> a of
                    [] -> throwStringErr "no input"
                    xs -> atomList xs
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "slurp" $ \case
            [MalString filename] -> do
                result <- liftIO . try $ readFile filename
                case result of
                    Left err -> throwStringErr $ show (err :: IOException)
                    Right content -> return $ MalString content
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "eval" $ \case
            [d] -> case atom2Mal d of
                Just a -> eval a
                Nothing -> throwStringErr "parse error"
            _ -> throwStringErr "InvalidArgNumber",
        func "type" $ \case
            [x] -> malString $ atomType x
            _ -> throwStringErr "ValueError",
        func "exit" $ \case
            [] -> liftIO exitSuccess
            [Number 0] -> liftIO exitSuccess
            [Number a] -> liftIO . exitWith $ ExitFailure (fromInteger a)
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "concat" concatList,
        func "err?" $ \case
            [AtomError _ _] -> bool True
            [_] -> bool False
            _ -> throwStringErr "InvalidArgNumber",
        func "err-value" $ \case
            [AtomError v _] -> return v
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber",
        func "traceback" $ \case
            [AtomError _ t] -> return . AtomList $ fmap MalString t
            [_] -> throwStringErr "ValueError"
            _ -> throwStringErr "InvalidArgNumber"
    ]

defaultEnv :: IO Env
defaultEnv = do
    (_, env) <- runEval preset emptyEnv
    return env

preset :: Eval ()
preset = modifyEnv (pushDefn defaultDefn) >> void malImpl

malImpl :: Eval MalAtom
malImpl = do
    evil "(def! (not a) (if a false true))"
    evil "(def! zero 0)"
    evil "(def! (succ n) (+ n 1))"
    evil "(def! (read-string x) (car [read-string-many x]))"
    evil "(def! (mal-string x) (car [mal-string-many x]))"
    evil "(def! (eval-many x) (car [read-string-many x]))"
    evil "(def! (load-file s) (let* [content (slurp s)] (evil content)))"
    evil "(def! first car) (def! rest cdr)"
    evil "(def! (nth xs n) [if (= n 0) (car xs) (nth (cdr xs) (- n 1))])"
    evil "(defmacro! cond(fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
