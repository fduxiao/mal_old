{-# LANGUAGE LambdaCase #-}
module Core where

import AST
import Env
import Eval
import Reader
import Control.Monad

-- global defines

-- arithmetic
addAtom :: MalAtom -> MalAtom -> Eval MalAtom
addAtom (Number n1) (Number n2) = number (n1 + n2)
addAtom (Number n1) (Floating n2) = floating (fromInteger n1 + n2)
addAtom (Floating n1) (Number n2) = floating (n1 + fromInteger n2)
addAtom (Floating n1) (Floating n2) = floating (n1 + n2)
addAtom a b = throwAtom . MalString $ "You can not add " ++ show a ++ " and " ++ show b

minusAtom :: MalAtom -> MalAtom -> Eval MalAtom
minusAtom (Number n1) (Number n2) = number (n1 - n2)
minusAtom (Number n1) (Floating n2) = floating (fromInteger n1 - n2)
minusAtom (Floating n1) (Number n2) = floating (n1 - fromInteger n2)
minusAtom (Floating n1) (Floating n2) = floating (n1 - n2)
minusAtom a b = throwAtom . MalString $ "You can not minus " ++ show a ++ " and " ++ show b

timesAtom :: MalAtom -> MalAtom -> Eval MalAtom
timesAtom (Number n1) (Number n2) = number (n1 * n2)
timesAtom (Number n1) (Floating n2) = floating (fromInteger n1 * n2)
timesAtom (Floating n1) (Number n2) = floating (n1 * fromInteger n2)
timesAtom (Floating n1) (Floating n2) = floating (n1 * n2)
timesAtom a b = throwAtom . MalString $ "You can not times " ++ show a ++ " and " ++ show b

divideAtom :: MalAtom -> MalAtom -> Eval MalAtom
divideAtom _ (Number 0) = throwAtom $ Number 0
divideAtom _ (Floating 0) = throwAtom $ Floating 0
divideAtom (Number n1) (Number n2)
    | n1 `mod` n2 == 0 = number (n1 `div` n2)
    | otherwise        = floating (fromInteger n1 / fromInteger n2)
divideAtom (Floating n1) (Number n2) = floating (n1 / fromInteger n2)
divideAtom (Number n1) (Floating n2) = floating (fromInteger n1 /  n2)
divideAtom (Floating n1) (Floating n2) = floating (n1 / n2)
divideAtom a b = throwAtom . MalString $ "You can not divide " ++ show a ++ " by " ++ show b

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = join (f <$> a <*> b)

addMany :: [MalAtom] -> Eval MalAtom
addMany [] = throwAtom $ MalString "Inadequate Operands"
addMany xs = foldl (bind2 addAtom) (number 0) $ fmap return xs

minusMany :: [MalAtom] -> Eval MalAtom
minusMany [] = throwAtom $ MalString "Inadequate Operands"
minusMany [x] = minusAtom (Number 0) x
minusMany (x:xs) = addMany xs >>= minusAtom x

timesMany :: [MalAtom] -> Eval MalAtom
timesMany [] = throwAtom $ MalString "Inadequate Operands"
timesMany xs = foldl (bind2 timesAtom) (number 1) $ fmap return xs

divideMany :: [MalAtom] -> Eval MalAtom
divideMany [] = throwAtom $ MalString "Inadequate Operands"
divideMany [x] = divideAtom (Number 1) x
divideMany (x:xs) = timesMany xs >>= divideAtom x

eq :: MalAtom -> MalAtom -> Eval Bool
eq (Func _ _) _ = throw ValueError
eq _ (Func _ _) = throw ValueError
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
lessThan _ _ = throw ValueError

lessThanEq :: MalAtom -> MalAtom -> Eval Bool
lessThanEq a b = (||) <$> eq a b <*> lessThan a b

greaterThan :: MalAtom -> MalAtom -> Eval Bool
greaterThan a b = not <$> lessThanEq a b

greaterThanEq :: MalAtom -> MalAtom -> Eval Bool
greaterThanEq a b = not <$> lessThan a b

compareMany :: (MalAtom -> MalAtom -> Eval Bool) -> [MalAtom] -> Eval MalAtom
compareMany _ [] = throw InvalidArgNumber
compareMany _ [x] = bool True
compareMany p (a:b:xs) = do
    r <- p a b
    if r then compareMany p (b:xs) else bool False

symbols :: Eval MalAtom
symbols = do
    env <- getEnv
    liftIO . print $ mergeDefn env
    return Nope

func :: String -> ([MalAtom] -> Eval MalAtom) -> (String, MalAtom)
func name body = (name, Func (Just name) body)

evil :: String -> Eval MalAtom
evil s = case parse (contents tops) s of 
        r@(Left _, _) -> throw . SyntaxError $ showResult r
        (Right a, _) -> eval $ Do a

-- global define
defaultDefn :: Defn
defaultDefn = defnFromList [
        func "+" addMany,
        func "-" minusMany,
        func "*" timesMany,
        func "/" divideMany,
        func "throw" $ \case
            [x] -> throwAtom x
            _ -> throwAtom $ MalString "Wrong arguments number",
        func "symbols" $ const symbols,
        func "prn" $ \xs -> liftIO $ mapM_ print xs >> return Nope,
        func "list" list,
        func "cons" $ \case
            [a, AtomList xs] -> list $ a:xs
            [a, _] -> throw TypeError
            _ -> throw InvalidArgNumber,
        func "car" $ \case
            [AtomList (x:_)] -> return x
            _ -> throw TypeError,
        func "cdr" $ \case
            [AtomList (_:xs)] -> list xs
            _ -> throw TypeError,
        func "list?" $ \case
            [AtomList _] -> bool True
            [_] -> bool False 
            _ -> throw InvalidArgNumber,
        func "count" $ \case
            [AtomList xs] -> number . sum . fmap (const 1) $ xs
            _ -> throw TypeError,
        func "empty?" $ \case
            [AtomList []] -> bool True
            [_] -> bool False 
            _ -> throw InvalidArgNumber,
        func "=" $ compareMany eq,
        func "<" $ compareMany lessThan,
        func "<=" $ compareMany lessThanEq,
        func ">" $ compareMany greaterThan,
        func ">=" $ compareMany greaterThanEq,
        func "evil" $ \case
            [MalString s] -> evil s
            _ -> throw ValueError
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
