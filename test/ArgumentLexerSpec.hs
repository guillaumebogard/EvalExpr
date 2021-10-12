module ArgumentLexerSpec ( spec ) where

import Test.Hspec        ( Spec
                         , it
                         , shouldBe )

import ArgumentLexer     ( Token(..)
                         , tokenizeArgs )

spec :: Spec
spec = do
    it "[tokenizeArgs]: One short help flag" $ tokenizeArgs ["-h"]        `shouldBe` [HELP]
    it "[tokenizeArgs]: One long help flag"  $ tokenizeArgs ["--help"]    `shouldBe` [HELP]
    it "[tokenizeArgs]: One expression"      $ tokenizeArgs ["(3*5)+2"]   `shouldBe` [EXPRESSION "(3*5)+2"]
    it "[tokenizeArgs]: Expression and flag" $ tokenizeArgs ["-h", "6*8"] `shouldBe` [HELP, EXPRESSION "6*8"]
