--
-- EvalExpr
-- File description:
-- Main
--

module Main                                             ( main ) where

import System.Environment                               ( getArgs )
import System.Exit                                      ( ExitCode(ExitFailure)
                                                        , exitWith
                                                        , exitSuccess
                                                        )
import Control.Exception                                ( Handler(..)
                                                        , catches
                                                        )

import qualified Argument.Parser                 as AP  ( parse )
import qualified Expression.Parser               as EP  ( parse )
import qualified Expression.Evaluation           as EE  ( evaluate )
import qualified Expression.Display              as ED  ( display )

import qualified Argument.Parser.Exception       as APE ( ArgumentParserException( ArgumentParserHelpException ) )
import qualified Expression.Lexer.Exception      as ELE ( ExpressionLexerException )
import qualified Expression.Parser.Exception     as EPE ( ExpressionParserException )
import qualified Expression.Evaluation.Exception as EEE ( ExpressionEvaluationException )

main :: IO ()
main = (getArgs >>= launchEvalExpr) `catches` [ Handler exceptionHandlerAPE
                                              , Handler exceptionHandlerELE
                                              , Handler exceptionHandlerEPE
                                              , Handler exceptionHandlerEEE
                                              ]

launchEvalExpr :: [String] -> IO ()
launchEvalExpr = ED.display . EE.evaluate . EP.parse . AP.parse

exceptionHandlerAPE :: APE.ArgumentParserException -> IO ()
exceptionHandlerAPE APE.ArgumentParserHelpException = print APE.ArgumentParserHelpException >> exitSuccess
exceptionHandlerAPE exception                       = print exception                       >> exitWith (ExitFailure 84)

exceptionHandlerELE :: ELE.ExpressionLexerException -> IO ()
exceptionHandlerELE                                 = flip (>>) (exitWith (ExitFailure 84)) . print

exceptionHandlerEPE :: EPE.ExpressionParserException -> IO ()
exceptionHandlerEPE                                 = flip (>>) (exitWith (ExitFailure 84)) . print

exceptionHandlerEEE :: EEE.ExpressionEvaluationException -> IO ()
exceptionHandlerEEE                                 = flip (>>) (exitWith (ExitFailure 84)) . print
