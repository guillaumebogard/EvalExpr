--
-- EvalExpr
-- File description:
-- Expression.EvaluationSpec
--

module Expression.EvaluationSpec                         ( spec ) where

import qualified Test.Hspec                      as TH   ( Spec
                                                         , it
                                                         , shouldThrow
                                                         )
import qualified Control.Exception               as CE   ( evaluate )
import qualified GHC.Float                       as GHCF ( powerDouble )

import qualified Expression.Parser               as EP   ( UnaryOperator(..)
                                                         , BinaryOperator(..)
                                                         , ExpressionTree(..)
                                                         )
import qualified Expression.Evaluation           as EE   ( EvaluationResult( EvaluationResult )
                                                         , evaluate
                                                         )

import qualified Expression.Evaluation.Exception as EEE  ( ExpressionEvaluationException( ExpressionEvaluationException ) )


newtype TestEvaluationResult = TestEvaluationResult EE.EvaluationResult


instance Eq TestEvaluationResult where
    (TestEvaluationResult (EE.EvaluationResult left)) == (TestEvaluationResult (EE.EvaluationResult right)) = left == right

spec :: TH.Spec
spec = do
    TH.it "Valid expression: \"1+1\"" $
        TestEvaluationResult (EE.evaluate (
            EP.BinaryNode
                EP.Addition
                (EP.Leaf 1)
                (EP.Leaf 1)
        )) == TestEvaluationResult (EE.EvaluationResult (1 + 1))
    TH.it "Valid expression: \"2*3+1\"" $
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
    TH.it "Valid expression: \"2*(3+1)\"" $
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
    TH.it "Valid expression: \"(2+3)*1\"" $
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
    TH.it "Valid expression: \"(2+3)*(1+10/(1+1))\"" $
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
    TH.it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1\"" $
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
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * (1.0 + 10.0 / (1.1 + 1.0)) `GHCF.powerDouble` 2.1))
    TH.it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1/1\"" $
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
        )) == TestEvaluationResult (EE.EvaluationResult ((2.0 + 3.0) * (1.0 + 10.0 / (1.1 + 1.0)) `GHCF.powerDouble` 2.1 / 1.0))
    TH.it "Valid expression: \"1/-++1\"" $
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
    TH.it "Invalid expression: \"1/(1-1)\"" $
        CE.evaluate (EE.evaluate (
            EP.BinaryNode
                EP.Division
                (EP.Leaf 1)
                (EP.BinaryNode
                    EP.Substraction
                    (EP.Leaf 1)
                    (EP.Leaf 1)
                )
        )) `TH.shouldThrow` (== EEE.ExpressionEvaluationException "Forbidden operation: Division by zero")
