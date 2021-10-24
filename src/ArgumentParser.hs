module ArgumentParser    ( Expression(..)
                         , parseArgs
                         ) where

import Control.Exception ( throw )

import Error             ( Error(HelpError, ArgumentParserError) )

import ArgumentLexer     ( Token(..)
                         , tokenizeArgs )

newtype Expression = Expression String

parseArgs :: [String] -> Expression
parseArgs = parseTokenizedArgs . tokenizeArgs

parseTokenizedArgs :: [Token] -> Expression
parseTokenizedArgs [EXPRESSION x] = Expression x
parseTokenizedArgs (HELP:_)       = throw HelpError
parseTokenizedArgs []             = throw $ ArgumentParserError "Too few arguments"
parseTokenizedArgs ((EXPRESSION _):xs)
    | HELP `elem` xs              = throw HelpError
    | otherwise                   = throw $ ArgumentParserError "Too many arguments"
