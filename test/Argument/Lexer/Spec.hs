--
-- EvalExpr
-- File description:
-- Argument.Lexer.Spec
--

module Argument.Lexer.Spec            ( spec ) where

import qualified Test.Hspec     as TH ( Spec
                                      , it
                                      )

import qualified Argument.Lexer as AL ( Token(..)
                                      , tokenize
                                      )


newtype TestToken = TestToken AL.Token


instance Eq TestToken where
    (TestToken AL.Help)                 == (TestToken AL.Help) = True
    (TestToken (AL.UnknownOption left)) == (TestToken (AL.UnknownOption right)) = left == right
    (TestToken (AL.Expression    left)) == (TestToken (AL.Expression    right)) = left == right
    _                                   == _                                    = False

spec :: TH.Spec
spec = do
    TH.it "Single short help option" $
        map TestToken (AL.tokenize ["-h"])
            == map TestToken [
                AL.Help
            ]
    TH.it "Single long help option" $
        map TestToken (AL.tokenize ["--help"])
            == map TestToken [
                AL.Help
            ]
    TH.it "Multiple short & long help options" $
        map TestToken (AL.tokenize ["-h", "--help"])
            == map TestToken [
                AL.Help
              , AL.Help
            ]
    TH.it "Single expression given" $
        map TestToken (AL.tokenize ["1+2"])
            == map TestToken [
                AL.Expression "1+2"
            ]
    TH.it "Single expression given with short help option" $
        map TestToken (AL.tokenize ["1+2", "-h"])
            == map TestToken [
                AL.Expression "1+2"
              , AL.Help
            ]
    TH.it "Single expression given with unknown option" $
        map TestToken (AL.tokenize ["1+2", "-a"])
            == map TestToken [
                AL.Expression "1+2"
              , AL.UnknownOption "-a"
            ]
