module ArgumentLexer ( Token(..)
                     , tokenizeArgs
                     ) where

data Token = HELP
           | EXPRESSION String
           deriving Eq

tokenizeArgs :: [String] -> [Token]
tokenizeArgs []            = []
tokenizeArgs ("-h":xs)     = HELP         : tokenizeArgs xs
tokenizeArgs ("--help":xs) = HELP         : tokenizeArgs xs
tokenizeArgs (x:xs)        = EXPRESSION x : tokenizeArgs xs
