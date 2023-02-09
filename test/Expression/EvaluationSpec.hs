--
-- EvalExpr
-- File description:
-- Expression.EvaluationSpec
--
{-# LANGUAGE InstanceSigs #-}

module           Expression.EvaluationSpec              ( spec ) where

import           Test.Hspec                             ( Spec
                                                        , it
                                                        , shouldThrow
                                                        )
import           Control.Exception                      ( evaluate )
import           GHC.Float                              ( powerDouble )

import qualified Expression.Parser               as EP  ( UnaryOperator(..)
                                                        , BinaryOperator(..)
                                                        , ExpressionTree(..)
                                                        )
import qualified Expression.Evaluation           as EE  ( EvaluationResult( EvaluationResult )
                                                        , evaluate
                                                        )

import qualified Expression.Evaluation.Exception as EEE ( ExpressionEvaluationException( ExpressionEvaluationException ) )


newtype TestEvaluationResult = TestEvaluationResult EE.EvaluationResult


instance Eq TestEvaluationResult where
    (==) :: TestEvaluationResult -> TestEvaluationResult -> Bool
    (TestEvaluationResult (EE.EvaluationResult left)) == (TestEvaluationResult (EE.EvaluationResult right)) = left == right

spec :: Spec
spec = do
    it "Valid expression: \"1+1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Addition
                (EP.Leaf 1)
                (EP.Leaf 1)
        )) == TestEvaluationResult (EE.EvaluationResult (1 + 1))
    it "Valid expression: \"2*3+1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Addition
                (EP.BinaryNode
                    EP.Multiplication
                    (EP.Leaf 2)
                    (EP.Leaf 3)
                )
                (EP.Leaf 1)
        )) == TestEvaluationResult (EE.EvaluationResult (2.0 * 3.0 + 1.0))
    it "Valid expression: \"2*(3+1)\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Multiplication
                (EP.Leaf 2)
                (EP.ProtectedNode
                    (EP.BinaryNode
                        EP.Addition
                        (EP.Leaf 3)
                        (EP.Leaf 1)
                    )
                )
        )) == TestEvaluationResult (EE.EvaluationResult (2.0 *(3.0 + 1.0)))
    it "Valid expression: \"(2+3)*1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Multiplication
                (EP.ProtectedNode
                    (EP.BinaryNode
                        EP.Addition
                        (EP.Leaf 2)
                        (EP.Leaf 3)
                    )
                )
                (EP.Leaf 1)
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * 1.0))
    it "Valid expression: \"(2+3)*(1+10/(1+1))\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Multiplication
                (EP.ProtectedNode
                    (EP.BinaryNode
                        EP.Addition
                        (EP.Leaf 2)
                        (EP.Leaf 3)
                    )
                )
                (EP.ProtectedNode
                    (EP.BinaryNode
                        EP.Addition
                        (EP.Leaf 1)
                        (EP.BinaryNode
                            EP.Division
                            (EP.Leaf 10)
                            (EP.ProtectedNode
                                (EP.BinaryNode
                                    EP.Addition
                                    (EP.Leaf 1)
                                    (EP.Leaf 1)
                                )
                            )
                        )
                    )
                )
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * (1.0 + 10.0 / (1.0 + 1.0))))
    it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Multiplication
                (EP.ProtectedNode
                    (EP.BinaryNode
                        EP.Addition
                        (EP.Leaf 2)
                        (EP.Leaf 3)
                    )
                )
                (EP.BinaryNode
                    EP.Power
                    (EP.ProtectedNode
                        (EP.BinaryNode
                            EP.Addition
                            (EP.Leaf 1)
                            (EP.BinaryNode
                                EP.Division
                                (EP.Leaf 10)
                                (EP.ProtectedNode
                                    (EP.BinaryNode
                                        EP.Addition
                                        (EP.Leaf 1.1)
                                        (EP.Leaf 1)
                                    )
                                )
                            )
                        )
                    )
                    (EP.Leaf 2.1)
                )
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * (1.0 + 10.0 / (1.1 + 1.0)) `powerDouble` 2.1))
    it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1/1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Division
                (EP.BinaryNode
                    EP.Multiplication
                    (EP.ProtectedNode
                        (EP.BinaryNode
                            EP.Addition
                            (EP.Leaf 2)
                            (EP.Leaf 3)
                        )
                    )
                    (EP.BinaryNode
                        EP.Power
                        (EP.ProtectedNode
                            (EP.BinaryNode
                                EP.Addition
                                (EP.Leaf 1)
                                (EP.BinaryNode
                                    EP.Division
                                    (EP.Leaf 10)
                                    (EP.ProtectedNode
                                        (EP.BinaryNode
                                            EP.Addition
                                            (EP.Leaf 1.1)
                                            (EP.Leaf 1)
                                        )
                                    )
                                )
                            )
                        )
                        (EP.Leaf 2.1)
                    )
                )
                (EP.Leaf 1)
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * (1.0 + 10.0 / (1.1 + 1.0)) `powerDouble` 2.1 / 1.0))
    it "Valid expression: \"1/-++1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Division
                (EP.Leaf 1)
                (EP.UnaryNode
                    EP.Minus
                    (EP.UnaryNode
                        EP.Plus
                        (EP.UnaryNode
                            EP.Plus
                            (EP.Leaf 1)
                        )
                    )
                )
        )) == TestEvaluationResult (EE.EvaluationResult (1.0 / (- 1.0)))
    it "Invalid expression: \"1/(1-1)\"" $
        evaluate (EE.evaluate (
            EP.BinaryNode
                EP.Division
                (EP.Leaf 1)
                (EP.BinaryNode
                    EP.Substraction
                    (EP.Leaf 1)
                    (EP.Leaf 1)
                )
        )) `shouldThrow` (== EEE.ExpressionEvaluationException "Forbidden operation: Division by zero")
