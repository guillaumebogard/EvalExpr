--
-- EvalExpr
-- File description:
-- Expression.Evaluation.Spec
--

module Expression.Evaluation.Spec                       ( spec ) where

import qualified Test.Hspec                      as TH  ( Spec
                                                        , it
                                                        , shouldThrow
                                                        )

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
    (TestEvaluationResult (EE.EvaluationResult left)) == (TestEvaluationResult (EE.EvaluationResult right)) = left == right

spec :: TH.Spec
spec = do
    TH.it "Simple valid expression: \"1+1\"" $
        TestEvaluationResult (
            EE.evaluate (
                EP.BinaryNode
                    EP.Addition
                    (EP.Leaf 1)
                    (EP.Leaf 1)
            )
        ) == TestEvaluationResult (EE.EvaluationResult 2)
