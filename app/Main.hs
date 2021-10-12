module Main               ( main ) where

import Control.Exception  ( handle )
import System.Environment ( getArgs )
import System.Exit        ( ExitCode(ExitFailure)
                          , exitWith )

import ArgumentParser     ( Conf(..)
                          , parseArgs )
import Error              ( Error(..) )

main :: IO ()
main = handle handleErrors (getArgs >>= launchApp . parseArgs)

launchApp :: Either Error Conf -> IO ()
launchApp (Left  _) = putStrLn "Error"
launchApp (Right _) = putStrLn "Right Conf"

handleErrors :: Error -> IO ()
handleErrors (Error e) = putStrLn e >> exitWith (ExitFailure 84)
