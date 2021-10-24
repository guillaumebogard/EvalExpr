module Main                       ( main ) where

import Test.Hspec                 ( hspec
                                  , Spec
                                  , describe )

import ArgumentLexerSpec          ( spec )
import ArgumentParserSpec         ( spec )
import ExpressionLexerSpec        ( spec )
import ExpressionParserSpec       ( spec )
import ExpressionTreeEvaluateSpec ( spec )
import IntegrationSpec            ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "ArgumentLexer"          ArgumentLexerSpec.spec
    describe "ArgumentParser"         ArgumentParserSpec.spec
    describe "ExpressionLexer"        ExpressionLexerSpec.spec
    describe "ExpressionParser"       ExpressionParserSpec.spec
    describe "ExpressionTreeEvaluate" ExpressionTreeEvaluateSpec.spec
    describe "Integration"            IntegrationSpec.spec
