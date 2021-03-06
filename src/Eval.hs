{-# LANGUAGE LambdaCase #-}
module Eval (
    module Eval,
    module AST,
    module Env
) where

import AST
import Env

catch :: Eval a -> (MalAtom -> Eval a) -> Eval a
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
bindArgs (MalList (Var "&":_)) _ = throwStringErr "InvalidArgNumber"
bindArgs (MalList (Var x:xs)) (a:as) = modifyEnv (setDefn x a) >> bindArgs (MalList xs) as
bindArgs _ _ = throwStringErr "InvalidArgNumber"

parseName :: Maybe String -> String
parseName Nothing = "lambda"
parseName (Just s) = s

unsetVar :: [Mal] -> Eval MalAtom
unsetVar [Var name] = modifyEnv (unsetDefn name) >> return Nil
unsetVar (Var name:xs) = modifyEnv (unsetDefn name) >> unsetVar xs
unsetVar _ = throwStringErr "ValueError"

reduce :: [Mal] -> Eval Mal

reduce [] = atom $ AtomList []
reduce (x:xs) = do
    f <- eval x
    r <- case f of 
        (Func n a) -> withCallStack (parseName n) $ do
            args <- evalArgs xs
            catch (a args) except where
                evalArgs :: [Mal] -> Eval [MalAtom]
                evalArgs [] = return []
                evalArgs (Var "&":b:rest) = eval b >>= \case
                    (AtomList xs) -> (xs++) <$> evalArgs rest
                    _ -> throwStringErr "ValueError"
                evalArgs (x:xs) = (:) <$> eval x <*> evalArgs xs
                except :: MalAtom -> Eval MalAtom
                except (AtomError value traceback) = throw $ AtomError value (traceback ++ [parseName n])
                except value = throw $ AtomError value [parseName n]
        (Macro n macro) -> withCallStack n $ do
            let args = fmap mal2Atom xs
            result <- macro args
            catch (eval result) except where
                except :: MalAtom -> Eval MalAtom
                except (AtomError value traceback) = throw $ AtomError value (traceback ++ [n])
                except value = throw $ AtomError value [n]
        _ -> throwStringErr $ "NotAFunction" ++ show f
    atom r

quasiquote :: Mal -> Eval MalAtom
quasiquote (MalList [Var "unquote", a]) = eval a
quasiquote (MalList (Var "unquote":_)) = throwStringErr "InvalidArgNumber"
quasiquote (MalList (MalList [Var "splice-unquote", expr]:xs)) = do
    array <- eval expr
    rest <- quasiquote (MalList xs)
    eval $ MalList [Var "concat", MalAtom array, MalAtom rest]

quasiquote (MalList (MalList (Var "splice-unquote":_):xs)) = throwStringErr "InvalidArgNumber"
quasiquote (MalList (x:xs)) = do
    first <- quasiquote x
    rest <- quasiquote $ MalList xs
    eval $ MalList [Var "cons", MalAtom first, MalAtom rest]
quasiquote a = eval $ Quote a

eval :: Mal -> Eval MalAtom
eval (MalAtom a) = return a
eval (Var a) = do
    env <- getEnv
    case find env a of
        Nothing -> throwStringErr $ "UndefinedSymbol" ++ a
        Just a -> return a

eval (MalDef name expr) = do
    r <- eval expr
    r <- return $ case r of 
        Func Nothing body -> Func (Just name) body 
        _ -> r
    modifyEnv $ setDefn name r
    return r

eval (Unset xs) = unsetVar xs

eval (Let xs expr) = withNewDefn (bindDefn xs >> eval expr)
eval (Do [x]) = eval x
eval (Do (x:xs)) = eval x >> eval (Do xs)

eval (If cond t f) = do
    c <- eval cond
    case c of
        Boolean False -> eval f
        Nil -> eval f
        _ -> eval t

eval (Fn params body) = return . Func Nothing $ \args -> withNewDefn $ do
    bindArgs params args
    env <- getEnv
    value <- eval body
    return $ case value of
        (Func a b) -> Func a $ \args -> do
            putEnv env
            b args
        x -> x

eval (MacroDef name v) = do
    body <- eval v
    case body of
        (Func _ proc) -> modifyEnv (setDefn name macro) >> return macro where
            macro = Macro name $ \args -> do
                result <- proc args
                case result of
                    AtomList ast -> case atom2Mal (AtomList ast) of
                        Nothing -> throw body
                        (Just a) -> return a
                    a -> atom a
        _ -> throw body

eval (Quote ast) = return $ mal2Atom ast
eval (QuasiQuote ast) = quasiquote ast

eval (Try body var caught) = catch (eval body) $ \err -> withNewDefn $ do
    modifyEnv $ setDefn var err
    eval caught

eval (MalList xs) = do
    d <- reduce xs
    case d of
        MalAtom a -> return a
        b -> eval b
