module Env where

import qualified Data.Map as Map
import AST

defnFromList :: [(String, MalAtom)] ->  Defn
defnFromList = Map.fromList

pushDefn :: Defn -> Env -> Env
pushDefn n env = let d = n:defn env in env {defn = d}

pushEmptyDefn :: Env -> Env
pushEmptyDefn = pushDefn Map.empty

popDefn :: Env -> Env
popDefn r@Env{defn=[]} = r
popDefn r@Env{defn=(x:xs)} = r {defn = xs}


findDefn :: [Defn] -> String -> Maybe MalAtom
findDefn [] _ = Nothing
findDefn (x:xs) key = case Map.lookup key x of
    Just a -> Just a
    Nothing -> findDefn xs key

find :: Env -> String -> Maybe MalAtom
find = findDefn . defn

setDefn :: String -> MalAtom -> Env -> Env
setDefn key value env@Env{defn=defn} = env {defn = set defn key value}
    where
        set :: [Defn] -> String -> MalAtom -> [Defn]
        set[] key value = [Map.insert key value Map.empty]
        set (x:xs) key value = Map.insert key value x:xs

mergeDefn :: Env -> Defn
mergeDefn Env{defn=defn} = foldl Map.union Map.empty defn


pushCallStack :: CallStack -> Env -> Env
pushCallStack s env@Env{callStack=stack} = env {callStack = s:stack}

popCallStack :: Env -> Env
popCallStack env@Env{callStack=[]} = env
popCallStack env@Env{callStack=x:xs} = env{callStack = xs}


pushTraceback :: Traceback -> Env -> Env
pushTraceback s env@Env{traceback=stack} = env {traceback = s:stack}

popTraceback :: Env -> Env
popTraceback env@Env{traceback=[]} = env
popTraceback env@Env{traceback=x:xs} = env{traceback = xs}

clearTraceback :: Env -> Env
clearTraceback env = env{traceback=[]}
