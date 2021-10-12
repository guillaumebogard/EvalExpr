module ArgumentParser ( Expression(..)
                      , parseArgs
                      ) where

import Control.Exception ( throw )

import Error          ( Error(..) )

import ArgumentLexer  ( Token(..)
                      , tokenizeArgs)

newtype Expression = Expression String deriving (Show, Eq)

parseArgs :: [String] -> Expression
parseArgs = parseTokenizedArgs . tokenizeArgs

parseTokenizedArgs :: [Token] -> Expression
parseTokenizedArgs [EXPRESSION x] = Expression x
parseTokenizedArgs (HELP:_)       = throw HelpError
parseTokenizedArgs []             = throw TooFewArgumentsError
parseTokenizedArgs ((EXPRESSION _):xs)
    | HELP `elem` xs              = throw HelpError 
    | otherwise                   = throw TooManyArgumentsError
