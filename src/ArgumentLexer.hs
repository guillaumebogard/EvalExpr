module ArgumentLexer ( Token(..)
                     , tokenizeArgs
                     ) where

data Token = HELP | EXPRESSION String deriving (Show, Eq)

tokenizeArgs :: [String] -> [Token]
tokenizeArgs ("-h":xs)     = HELP         : tokenizeArgs xs
tokenizeArgs ("--help":xs) = HELP         : tokenizeArgs xs
tokenizeArgs (x:xs)        = EXPRESSION x : tokenizeArgs xs
tokenizeArgs []            = []
