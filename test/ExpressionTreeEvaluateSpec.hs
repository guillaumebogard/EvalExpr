module ExpressionTreeEvaluateSpec ( spec ) where

import Test.Hspec                 ( Spec
                                  , it )

import ExpressionParser as EP     ( UnaryOperator(..)
                                  , BinaryOperator(..)
                                  , ExpressionTree(..) )
import ExpressionTreeEvaluate     ( evaluateExpressionTree )

spec :: Spec
spec = do
    it "Simple expression: 1+2" $ do
        evaluateExpressionTree (EP.BinaryNode EP.ADDITION (EP.Leaf 1) (EP.Leaf 2)) == 3.0
    it "Advanced expression: (1+2)*3^2" $ do
        evaluateExpressionTree (EP.BinaryNode EP.MULTIPLICATION (EP.ProtectedNode (EP.BinaryNode EP.ADDITION (EP.Leaf 1) (EP.Leaf 2))) (EP.BinaryNode EP.POWER (EP.Leaf 3) (EP.Leaf 2))) == 27.0
