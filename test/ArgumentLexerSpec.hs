module ArgumentLexerSpec   ( spec ) where

import Test.Hspec          ( Spec
                           , it
                           , shouldThrow )

import ArgumentLexer as AL ( Token(..)
                           , tokenizeArgs )

spec :: Spec
spec = do
    it "Single short help flag" $ do
        tokenizeArgs ["-h"] == [AL.HELP]
    it "Single long help flag" $ do
        tokenizeArgs ["--help"] == [AL.HELP]
    it "Multiple short & long help flags" $ do
        tokenizeArgs ["-h", "--help"] == [AL.HELP, AL.HELP]
    it "Expression given" $ do
        tokenizeArgs ["1+2"] == [AL.EXPRESSION "1+2"]
    it "Expression given & help flags" $ do
        tokenizeArgs ["1+2", "-h"] == [AL.EXPRESSION "1+2", AL.HELP]
