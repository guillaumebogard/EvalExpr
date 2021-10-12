module Main               ( main ) where

import Control.Exception  ( handle )
import System.Environment ( getArgs )
import System.Exit        ( ExitCode(ExitFailure, ExitSuccess)
                          , exitWith )

import ArgumentParser     ( Expression(..)
                          , parseArgs )
import Error              ( Error(..) )

main :: IO ()
main = handle handleErrors (getArgs >>= launchApp . parseArgs)

launchApp :: Expression -> IO ()
launchApp _ = putStrLn "Right Conf"

handleErrors :: Error -> IO ()
handleErrors (HelpError)             = putStrLn "Usage" >> exitWith ExitSuccess
handleErrors (TooManyArgumentsError) = putStrLn "Too Many Arguments" >> exitWith (ExitFailure 84)
handleErrors (TooFewArgumentsError)  = putStrLn "Too Few Arguments" >> exitWith (ExitFailure 84)
