--
-- EvalExpr
-- File description:
-- Argument.Lexer
--

module Argument.Lexer ( Token(..)
                      , tokenize
                      ) where


data Token = Help
           | UnknownOption String
           | Expression    String


tokenize :: [String] -> [Token]
tokenize []               = []
tokenize ("-h"       :xs) = Help              : tokenize xs
tokenize ("--help"   :xs) = Help              : tokenize xs
tokenize (opt@('-':_):xs) = UnknownOption opt : tokenize xs
tokenize (x          :xs) = Expression    x   : tokenize xs
