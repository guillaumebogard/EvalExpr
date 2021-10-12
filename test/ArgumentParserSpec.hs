module ArgumentParserSpec ( spec ) where

import Test.Hspec         ( Spec
                          , it
                          , shouldBe
                          , shouldThrow)

import Control.Exception  ( evaluate )

import ArgumentParser     ( Expression(..)
                          , parseArgs )
import Error              ( Error(..) )

spec :: Spec
spec = do
    it "[parseArgs]: One short help flag"                     $ evaluate (parseArgs ["-h"])                   `shouldThrow` (== HelpError)
    it "[parseArgs]: One long help flag"                      $ evaluate (parseArgs ["--help"])               `shouldThrow` (== HelpError)
    it "[parseArgs]: One short and one long help flag"        $ evaluate (parseArgs ["--help", "-h"])         `shouldThrow` (== HelpError)
    it "[parseArgs]: One short help flag and one expression"  $ evaluate (parseArgs ["-h", "(3*5)+8"])        `shouldThrow` (== HelpError)
    it "[parseArgs]: One expression and one short help flag"  $ evaluate (parseArgs ["(3*5)+8", "-h"])        `shouldThrow` (== HelpError)
    it "[parseArgs]: Two expressions"                         $ evaluate (parseArgs ["(3*5)+8", "6*8"])       `shouldThrow` (== TooManyArgumentsError)
    it "[parseArgs]: Two expressions and one short help flag" $ evaluate (parseArgs ["(3*5)+8", "6*8", "-h"]) `shouldThrow` (== HelpError)
    it "[parseArgs]: No arguments"                            $ evaluate (parseArgs [])                       `shouldThrow` (== TooFewArgumentsError)
    it "[parseArgs]: One expression"                          $ parseArgs ["(3*5)+8"]                         `shouldBe`    Expression "(3*5)+8"
