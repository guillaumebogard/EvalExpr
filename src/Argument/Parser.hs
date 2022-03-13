--
-- EvalExpr
-- File description:
-- Argument.Parser
--

module Argument.Parser                            ( Expression(..)
                                                  , parse
                                                  ) where

import Control.Exception                          ( throw )

import qualified Argument.Lexer            as AL  ( Token(..)
                                                  , tokenize
                                                  )

import qualified Argument.Parser.Exception as APE ( ArgumentParserException( ArgumentParserHelpException
                                                                           , ArgumentParserException
                                                                           )
                                                  )

newtype Expression = Expression String

parse :: [String] -> Expression
parse = parseTokens . AL.tokenize

parseTokens :: [AL.Token] -> Expression
parseTokens = checkTokensInLayers

checkTokensInLayers :: [AL.Token] -> Expression
checkTokensInLayers []     = throw $ APE.ArgumentParserException "Requires at least one expression, retry with '-h'"
checkTokensInLayers tokens = checkTokensInLayers' tokens

checkTokensInLayers' :: [AL.Token] -> Expression
checkTokensInLayers' = expressionHandler . filter helpHandler . filter unknownOptionHandler

helpHandler :: AL.Token -> Bool
helpHandler AL.Help = throw APE.ArgumentParserHelpException
helpHandler _       = True

unknownOptionHandler :: AL.Token -> Bool
unknownOptionHandler (AL.UnknownOption opt) = throw $ APE.ArgumentParserException $ "Unknown option: '" ++ opt ++ "'"
unknownOptionHandler _                      = True

expressionHandler:: [AL.Token] -> Expression
expressionHandler [AL.Expression expression] = Expression expression
expressionHandler _                          = throw $ APE.ArgumentParserException "Can only take one expression"
