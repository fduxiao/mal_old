{-# LANGUAGE LambdaCase #-}
module Eval (
    module Eval,
    module AST,
    module Env
) where

import AST
import Env

catch :: Eval a -> (EvalError -> Eval a) -> Eval a
catch (Eval ea) handle = Eval $ \env -> do
    (a, env') <- ea env
    case a of
        Left e -> runEval (handle e) env'
        Right a -> return (Right a, env')

withCallStack :: CallStack -> Eval a -> Eval a
withCallStack stack (Eval p) = Eval $ \env -> do
    (a, env') <- p (pushCallStack stack env)
    return (a, popCallStack env')

withNewDefn :: Eval a -> Eval a
withNewDefn (Eval p) = Eval $ \env -> do
    (a, env') <- p (pushEmptyDefn env)
    return (a, popDefn env')

copyEnv :: Eval a -> Eval a
copyEnv (Eval p) = Eval $ \env -> do
    (r, _) <- p env
    return (r, env)

bindDefn :: [(String, Mal)] -> Eval ()
bindDefn [] = return ()
bindDefn ((name, expr):rest) = do
    value <- eval expr
    modifyEnv $ setDefn name value
    bindDefn rest

bindArgs :: Mal -> [MalAtom] -> Eval ()
bindArgs (Var x) value = modifyEnv $ setDefn x (AtomList value)
bindArgs (MalList []) [] = return ()
bindArgs (MalList [Var "&", Var name]) xs = modifyEnv (setDefn name (AtomList xs)) -- the rest
bindArgs (MalList (Var "&":_)) _ = throw InvalidArgNumber
bindArgs (MalList (Var x:xs)) (a:as) = modifyEnv (setDefn x a) >> bindArgs (MalList xs) as
bindArgs _ _ = throw InvalidArgNumber

parseName :: Maybe String -> String
parseName Nothing = "lambda"
parseName (Just s) = s

reduce :: Mal -> Eval Mal
reduce (MalAtom a) = atom a
reduce (Var a) = do
    env <- getEnv
    case find env a of 
        Nothing -> throw $ UndefinedSymbol a
        Just a -> atom a

reduce (MalDef name expr) = do
    r <- eval expr
    r <- return $ case r of 
        Func Nothing body -> Func (Just name) body 
        _ -> r
    modifyEnv $ setDefn name r
    atom r

reduce (Let xs expr) = withNewDefn (bindDefn xs >> MalAtom <$> eval expr)
reduce (Do [x]) = MalAtom <$> eval x
reduce (Do (x:xs)) = eval x >> reduce (Do xs)

reduce (If cond t f) = do
    c <- eval cond
    case c of
        Boolean False -> MalAtom <$> eval f
        Nope -> MalAtom <$> eval f
        _ -> MalAtom <$> eval t

reduce (Fn params body) = atom . Func Nothing $ \args -> copyEnv $ do
    bindArgs params args
    env <- getEnv
    value <- eval body
    return $ case value of
        (Func a b) -> Func a $ \args -> do
            putEnv env
            b args
        x -> x

reduce (MalList []) = atom $ AtomList []
reduce (MalList (x:xs)) = do
    f <- eval x
    r <- case f of 
        (Func n a) -> withCallStack (parseName n) $ do
            modifyEnv $ pushTraceback (parseName n)
            args <- evalArgs xs
            a args where
                evalArgs :: [Mal] -> Eval [MalAtom]
                evalArgs [] = return []
                evalArgs (Var "&":b:rest) = eval b >>= \case
                    (AtomList xs) -> (xs++) <$> evalArgs rest
                    _ -> throw ValueError
                evalArgs (x:xs) = (:) <$> eval x <*> evalArgs xs
        _ -> throw $ NotAFunction $ show f
    modifyEnv popTraceback
    atom r

eval :: Mal -> Eval MalAtom
eval mal = do
    d <- reduce mal
    case d of
        MalAtom a -> return a
        b -> eval b
