module Lib
    ( rep
    ) where

import Lexer
import Parser

rep :: String -> IO ()
rep s = print $ parse (many token) s
