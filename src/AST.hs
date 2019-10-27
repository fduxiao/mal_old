module AST where

data EvalError = UndefinedSymbol String | Zero | NotAFunction String deriving(Show)

data MalAtom = 
    Symbol String 
    | MalString String
    | Number Integer 
    | Floating Float 
    | Nil
    | Boolean Bool
    | Func ([MalAtom] -> Mal)
    | Error MalAtom

instance Show MalAtom where
    show (Symbol s) = ':':s
    show (MalString s) = show s
    show (Number i) = show i
    show (Floating f) = show f
    show Nil = "nil"
    show (Boolean b) = if b then "true" else "false"
    show (Func f) = "proc"
    show (Error m) = "Except: " ++ show m

data Mal = MalAtom MalAtom | MalList [Mal] | Var String | Comment String
instance Show Mal where
    show (MalAtom a) = show a
    show (Var v) = v
    show (MalList l) = handle l where
        handle [] = "nil"
        handle (x:xs) = '(':show x ++ concatMap (\t -> ' ':show t) xs ++ ")"
    show (Comment c) = ""

atom :: (Monad m) => MalAtom -> m Mal
atom = return . MalAtom

addAtom :: MalAtom -> MalAtom -> MalAtom
addAtom (Number n1) (Number n2) = Number (n1 + n2)
addAtom (Number n1) (Floating n2) = Floating (fromInteger n1 + n2)
addAtom (Floating n1) (Number n2) = Floating (n1 + fromInteger n2)
addAtom (Floating n1) (Floating n2) = Floating (n1 + n2)
addAtom a b = Error . MalString $ "You can not add " ++ show a ++ " and " ++ show b


minusAtom :: MalAtom -> MalAtom -> MalAtom
minusAtom (Number n1) (Number n2) = Number (n1 - n2)
minusAtom (Number n1) (Floating n2) = Floating (fromInteger n1 - n2)
minusAtom (Floating n1) (Number n2) = Floating (n1 - fromInteger n2)
minusAtom (Floating n1) (Floating n2) = Floating (n1 - n2)
minusAtom a b = Error . MalString $ "You can not minus " ++ show a ++ " and " ++ show b

timesAtom :: MalAtom -> MalAtom -> MalAtom
timesAtom (Number n1) (Number n2) = Number (n1 * n2)
timesAtom (Number n1) (Floating n2) = Floating (fromInteger n1 * n2)
timesAtom (Floating n1) (Number n2) = Floating (n1 * fromInteger n2)
timesAtom (Floating n1) (Floating n2) = Floating (n1 * n2)
timesAtom a b = Error . MalString $ "You can not times " ++ show a ++ " and " ++ show b

divideAtom :: MalAtom -> MalAtom -> MalAtom
divideAtom _ (Number 0) = Error $ Number 0
divideAtom _ (Floating 0) = Error $ Floating 0
divideAtom (Number n1) (Number n2)
    | n1 `mod` n2 == 0 = Number (n1 `div` n2) 
    | otherwise        = Floating (fromInteger n1 / fromInteger n2)
divideAtom (Floating n1) (Number n2) = Floating (n1 / fromInteger n2)
divideAtom (Number n1) (Floating n2) = Floating (fromInteger n1 /  n2)
divideAtom (Floating n1) (Floating n2) = Floating (n1 / n2)
divideAtom a b = Error . MalString $ "You can not divide " ++ show a ++ " by " ++ show b

addMany :: [MalAtom] -> MalAtom
addMany [] = Error $ MalString "Inadequate Operands"
addMany xs = foldl addAtom (Number 0) xs

minusMany :: [MalAtom] -> MalAtom
minusMany [] = Error $ MalString "Inadequate Operands"
minusMany [x] = minusAtom (Number 0) x
minusMany (x:xs) = minusAtom x (addMany xs)

timesMany :: [MalAtom] -> MalAtom
timesMany [] = Error $ MalString "Inadequate Operands"
timesMany xs = foldl timesAtom (Number 1) xs

divideMany :: [MalAtom] -> MalAtom
divideMany [] = Error $ MalString "Inadequate Operands"
divideMany [x] = divideAtom (Number 1) x
divideMany (x:xs) = divideAtom x (timesMany xs)
