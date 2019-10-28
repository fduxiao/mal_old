module Env where


import qualified Data.Map as Map
import AST

type Defn = Map.Map String MalAtom
type Env = [(String, Defn)]

defnFromList :: [(String, MalAtom)] ->  Defn
defnFromList = Map.fromList

pushDefn :: String -> Defn -> Env -> Env
pushDefn n a = ((n, a):)

pushEmptyDefn :: String -> Env -> Env
pushEmptyDefn name env = (name, Map.empty):env

popDefn :: Env -> Env
popDefn [] = []
popDefn (x:xs) = xs

find :: Env -> String -> Maybe MalAtom
find [] _ = Nothing
find ((_, x):xs) key = case Map.lookup key x of
    Just a -> Just a
    Nothing -> find xs key

set :: Env -> String -> MalAtom -> Env
set [] key value = [("anonymous", Map.insert key value Map.empty)]
set ((n, x): xs) key value = (n, Map.insert key value x):xs
