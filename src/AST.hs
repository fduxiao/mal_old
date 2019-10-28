module AST where

data EvalError = UndefinedSymbol String | NotAFunction String | MalError MalAtom deriving(Show)

data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | Nil
    | Boolean Bool
    | Func String ([MalAtom] -> Either EvalError Mal)  -- name and its definition

instance Show MalAtom where
    show (Symbol s) = ':':s
    show (MalString s) = show s
    show (Number i) = show i
    show (Floating f) = show f
    show Nil = "nil"
    show (Boolean b) = if b then "true" else "false"
    show (Func n f) = "proc:" ++ n

data Mal = 
    MalAtom MalAtom
    | MalList [Mal] 
    | Var String 
    | Comment String

instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList l) = handle l where
        handle [] = "nil"
        handle (x:xs) = '(':show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (Comment c) = ""

atom :: (Monad m) => MalAtom -> m Mal
atom = return . MalAtom

addAtom :: MalAtom -> MalAtom -> Either EvalError MalAtom
addAtom (Number n1) (Number n2) = Right $ Number (n1 + n2)
addAtom (Number n1) (Floating n2) = Right $ Floating (fromInteger n1 + n2)
addAtom (Floating n1) (Number n2) = Right $ Floating (n1 + fromInteger n2)
addAtom (Floating n1) (Floating n2) = Right $ Floating (n1 + n2)
addAtom a b = Left . MalError . MalString $ "You can not add " ++ show a ++ " and " ++ show b


minusAtom :: MalAtom -> MalAtom -> Either EvalError MalAtom
minusAtom (Number n1) (Number n2) = Right $ Number (n1 - n2)
minusAtom (Number n1) (Floating n2) = Right $ Floating (fromInteger n1 - n2)
minusAtom (Floating n1) (Number n2) = Right $ Floating (n1 - fromInteger n2)
minusAtom (Floating n1) (Floating n2) = Right $ Floating (n1 - n2)
minusAtom a b = Left . MalError . MalString $ "You can not minus " ++ show a ++ " and " ++ show b

timesAtom :: MalAtom -> MalAtom -> Either EvalError MalAtom
timesAtom (Number n1) (Number n2) = Right $ Number (n1 * n2)
timesAtom (Number n1) (Floating n2) = Right $ Floating (fromInteger n1 * n2)
timesAtom (Floating n1) (Number n2) = Right $ Floating (n1 * fromInteger n2)
timesAtom (Floating n1) (Floating n2) = Right $ Floating (n1 * n2)
timesAtom a b = Left . MalError . MalString $ "You can not times " ++ show a ++ " and " ++ show b

divideAtom :: MalAtom -> MalAtom -> Either EvalError MalAtom
divideAtom _ (Number 0) = Left . MalError $ Number 0
divideAtom _ (Floating 0) = Left . MalError $ Floating 0
divideAtom (Number n1) (Number n2)
    | n1 `mod` n2 == 0 = Right $ Number (n1 `div` n2) 
    | otherwise        = Right $ Floating (fromInteger n1 / fromInteger n2)
divideAtom (Floating n1) (Number n2) = Right $ Floating (n1 / fromInteger n2)
divideAtom (Number n1) (Floating n2) = Right $ Floating (fromInteger n1 /  n2)
divideAtom (Floating n1) (Floating n2) = Right $ Floating (n1 / n2)
divideAtom a b = Left . MalError . MalString $ "You can not divide " ++ show a ++ " by " ++ show b

putEither :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
putEither f a b = do
    x <- a
    b >>= f x

addMany :: [MalAtom] -> Either EvalError MalAtom
addMany [] = Left . MalError $ MalString "Inadequate Operands"
addMany xs = foldl (putEither addAtom) (Right $ Number 0) $ fmap Right xs

minusMany :: [MalAtom] -> Either EvalError MalAtom
minusMany [] = Left . MalError $ MalString "Inadequate Operands"
minusMany [x] = minusAtom (Number 0) x
minusMany (x:xs) = addMany xs >>= minusAtom x

timesMany :: [MalAtom] -> Either EvalError MalAtom
timesMany [] = Left . MalError $ MalString "Inadequate Operands"
timesMany xs = foldl (putEither timesAtom) (Right $ Number 1) $ fmap Right xs

divideMany :: [MalAtom] -> Either EvalError MalAtom
divideMany [] = Left . MalError $ MalString "Inadequate Operands"
divideMany [x] = divideAtom (Number 1) x
divideMany (x:xs) = timesMany xs >>= divideAtom x
