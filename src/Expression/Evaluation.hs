--
-- EvalExpr
-- File description:
-- Expression.Evaluation
--

module Expression.Evaluation                             ( EvaluationResult(..)
                                                         , evaluate
                                                         ) where

import qualified GHC.Exception                   as GHCE ( throw )

import qualified Expression.Lexer                as EL   ( OperandType )
import qualified Expression.Parser               as EP   ( UnaryOperator(..)
                                                         , BinaryOperator(..)
                                                         , ExpressionTree(..)
                                                         )

import qualified Expression.Evaluation.Exception as EEE  ( ExpressionEvaluationException( ExpressionEvaluationException ) )


newtype EvaluationResult = EvaluationResult EL.OperandType


evaluate :: EP.ExpressionTree -> EvaluationResult
evaluate = EvaluationResult . evaluate'

evaluate' :: EP.ExpressionTree -> EL.OperandType
evaluate' (EP.Leaf          operand                        ) =   operand
evaluate' (EP.ProtectedNode subtree                        ) =   evaluate' subtree
evaluate' (EP.UnaryNode     EP.Plus           subtree      ) =   evaluate' subtree
evaluate' (EP.UnaryNode     EP.Minus          subtree      ) = - evaluate' subtree
evaluate' (EP.BinaryNode    EP.Addition       left    right) =   evaluate' left +  evaluate' right
evaluate' (EP.BinaryNode    EP.Substraction   left    right) =   evaluate' left -  evaluate' right
evaluate' (EP.BinaryNode    EP.Multiplication left    right) =   evaluate' left *  evaluate' right
evaluate' (EP.BinaryNode    EP.Division       left    right) =   evaluateDivision (evaluate' left) $ evaluate' right
evaluate' (EP.BinaryNode    EP.Power          left    right) =   evaluate' left ** evaluate' right

evaluateDivision :: EL.OperandType -> EL.OperandType -> EL.OperandType
evaluateDivision _    0     = GHCE.throw $ EEE.ExpressionEvaluationException "Forbidden operation: Division by zero"
evaluateDivision left right = left / right
