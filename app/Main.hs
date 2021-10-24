module Main                   ( main ) where

import Control.Exception      ( handle )
import System.Environment     ( getArgs )
import System.Exit            ( ExitCode(ExitFailure)
                              , exitWith
                              , exitSuccess )
import Text.Printf            ( printf )

import Error                  ( Error(..) )

import ArgumentParser         ( Expression(..)
                              , parseArgs )
import ExpressionParser       ( parseExpression )
import ExpressionTreeEvaluate ( evaluateExpressionTree )

main :: IO ()
main = handle handleError (getArgs >>= launchApp . parseArgs)

launchApp :: Expression -> IO ()
launchApp = printf "%.2f\n" . evaluateExpressionTree . parseExpression 

handleError :: Error -> IO ()
handleError HelpError                           = putStrLn usage   >> exitSuccess
handleError (ArgumentParserError       message) = putStrLn message >> exitWith (ExitFailure 84)
handleError (InvalidExpressionError    message) = putStrLn message >> exitWith (ExitFailure 84)
handleError (ExpressionEvaluationError message) = putStrLn message >> exitWith (ExitFailure 84)

usage :: String
usage = "USAGE: ./funEvalExpr expression\n\n\texpression\tA mathematical expression to be evaluated"
