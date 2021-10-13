module Main               ( main ) where

import Control.Exception  ( handle )
import System.Environment ( getArgs )
import System.Exit        ( ExitCode(ExitFailure)
                          , exitWith
                          , exitSuccess )

import ArgumentParser     ( Expression(..)
                          , parseArgs )
import Error              ( Error(..) )

main :: IO ()
main = handle handleErrors (getArgs >>= launchApp . parseArgs)

launchApp :: Expression -> IO ()
launchApp _ = putStrLn "Right Conf"

handleErrors :: Error -> IO ()
handleErrors HelpError              = putStrLn "Usage" >> exitSuccess
handleErrors TooManyArgumentsError  = putStrLn "Too Many Arguments" >> exitWith (ExitFailure 84)
handleErrors TooFewArgumentsError   = putStrLn "Too Few Arguments" >> exitWith (ExitFailure 84)
handleErrors InvalidExpressionError = putStrLn "Invalid Expression" >> exitWith (ExitFailure 84)
