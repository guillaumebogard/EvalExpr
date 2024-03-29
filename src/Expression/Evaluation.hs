--
-- EvalExpr
-- File description:
-- Expression.Evaluation
--

module           Expression.Evaluation                  ( EvaluationResult(..)
                                                        , evaluate
                                                        ) where

import           Control.Exception                      ( throw )

import qualified Expression.Lexer                as EL  ( OperandType )
import qualified Expression.Parser               as EP  ( UnaryOperator(..)
                                                        , BinaryOperator(..)
                                                        , ExpressionTree(..)
                                                        )

import qualified Expression.Evaluation.Exception as EEE ( ExpressionEvaluationException( ExpressionEvaluationException ) )


newtype EvaluationResult = EvaluationResult EL.OperandType


evaluate :: EP.ExpressionTree -> EvaluationResult
evaluate = EvaluationResult . evaluate'

evaluate' :: EP.ExpressionTree -> EL.OperandType
evaluate' (EP.Leaf          operand                     ) =   operand
evaluate' (EP.ProtectedNode down                        ) =   evaluate' down
evaluate' (EP.UnaryNode     EP.Plus           down      ) =   evaluate' down
evaluate' (EP.UnaryNode     EP.Minus          down      ) = - evaluate' down
evaluate' (EP.BinaryNode    EP.Addition       left right) =   evaluate' left +  evaluate' right
evaluate' (EP.BinaryNode    EP.Substraction   left right) =   evaluate' left -  evaluate' right
evaluate' (EP.BinaryNode    EP.Multiplication left right) =   evaluate' left *  evaluate' right
evaluate' (EP.BinaryNode    EP.Division       left right) =   evaluateDivision (evaluate' left) $ evaluate' right
evaluate' (EP.BinaryNode    EP.Power          left right) =   evaluate' left ** evaluate' right

evaluateDivision :: EL.OperandType -> EL.OperandType -> EL.OperandType
evaluateDivision _    0     = throw $ EEE.ExpressionEvaluationException "Forbidden operation: Division by zero"
evaluateDivision left right = left / right
