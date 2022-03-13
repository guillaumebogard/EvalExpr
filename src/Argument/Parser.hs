--
-- EvalExpr
-- File description:
-- Argument.Parser
--

module Argument.Parser                             ( Expression(..)
                                                   , parse
                                                   ) where

import qualified GHC.Exception             as GHCE ( throw )

import qualified Argument.Lexer            as AL   ( Token(..)
                                                   , tokenize
                                                   )

import qualified Argument.Parser.Exception as APE  ( ArgumentParserException( ArgumentParserHelpException
                                                                            , ArgumentParserException
                                                                            )
                                                   )

newtype Expression = Expression String

parse :: [String] -> Expression
parse = parseTokens . AL.tokenize

parseTokens :: [AL.Token] -> Expression
parseTokens = checkTokensInLayers

checkTokensInLayers :: [AL.Token] -> Expression
checkTokensInLayers []     = GHCE.throw $ APE.ArgumentParserException "Requires at least one expression, retry with '-h'"
checkTokensInLayers tokens = checkTokensInLayers' tokens

checkTokensInLayers' :: [AL.Token] -> Expression
checkTokensInLayers' = expressionHandler . filter helpHandler . filter unknownOptionHandler

helpHandler :: AL.Token -> Bool
helpHandler AL.Help = GHCE.throw APE.ArgumentParserHelpException
helpHandler _       = True

unknownOptionHandler :: AL.Token -> Bool
unknownOptionHandler (AL.UnknownOption opt) = GHCE.throw $ APE.ArgumentParserException $ "Unknown option: '" ++ opt ++ "'"
unknownOptionHandler _                      = True

expressionHandler:: [AL.Token] -> Expression
expressionHandler [AL.Expression expression] = Expression expression
expressionHandler _                          = GHCE.throw $ APE.ArgumentParserException "Can only take one expression"
