module IntegrationSpec        ( spec ) where

import Test.Hspec             ( Spec
                              , it )

import Control.Exception      ( evaluate )

import ArgumentParser         ( Expression(..)
                              , parseArgs )
import ExpressionLexer        ( OperandType )
import ExpressionParser       ( parseExpression )
import ExpressionTreeEvaluate ( evaluateExpressionTree )

launchIntegration :: [String] -> OperandType
launchIntegration = evaluateExpressionTree . parseExpression . parseArgs

spec :: Spec
spec = do
    it "Basic addition: 2+2" $ do
        launchIntegration ["2+2"] == 4.0
    it "Basic priorities: 2+2*2" $ do
        launchIntegration ["2+2*2"] == 6.0
    it "Basic priorities: 2+2+2+2" $ do
        launchIntegration ["2+2+2+2"] == 8.0
    it "Basic priorities: 2+2*2+1-2^2" $ do
        launchIntegration ["2+2*2+1-2^2"] == 3.0
    it "Basic parenthesis: 2+2*(2+1)-2^2" $ do
        launchIntegration ["2+2*(2+1)-2^2"] == 4.0
    it "Basic parenthesis: (2+2*2+1-2+5)^2" $ do
        launchIntegration ["(2+2*2+1-2+5)^2"] == 100.0
    it "Advanced priority: 1+2*3^2*2+2-4^5" $ do
        launchIntegration ["1+2*3^2*2+2-4^5"] == (-985.0)
    it "Advanced parenthesis: 1*2+(1+(3^(6/3)))" $ do
        launchIntegration ["1*2+(1+(3^(6/3)))"] == 12.0
    it "Basic minus: 3--2" $ do
        launchIntegration ["3--2"] == 5.0
    it "Basic minus: -5-6" $ do
        launchIntegration ["-5-6"] == -11.0
    it "Basic plus: 5++6-+1" $ do
        launchIntegration ["5++6-+1"] == 10.0
    it "Special input: -5" $ do
        launchIntegration ["-5"] == -5.0
    it "Special input: +5" $ do
        launchIntegration ["+5"] == 5.0
    it "Special input: (5)" $ do
        launchIntegration ["(5)"] == 5.0
    it "Special input: -(-5)" $ do
        launchIntegration ["-(-5)"] == 5.0
