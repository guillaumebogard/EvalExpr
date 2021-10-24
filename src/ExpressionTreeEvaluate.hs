module ExpressionTreeEvaluate ( evaluateExpressionTree ) where

import GHC.Exception          ( throw )

import Error                  ( Error(ExpressionEvaluationError) )

import ExpressionLexer        ( OperandType )
import ExpressionParser as EP ( UnaryOperator(..)
                              , BinaryOperator(..)
                              , ExpressionTree(..) )

evaluateExpressionTree :: ExpressionTree -> OperandType
evaluateExpressionTree (Leaf operand)                            = operand
evaluateExpressionTree (ProtectedNode                subtree)    = evaluateExpressionTree subtree
evaluateExpressionTree (UnaryNode  EP.PLUS           subtree)    = evaluateExpressionTree subtree
evaluateExpressionTree (UnaryNode  EP.MINUS          subtree)    = -(evaluateExpressionTree subtree)
evaluateExpressionTree (BinaryNode EP.ADDITION       left right) = evaluateExpressionTree left +  evaluateExpressionTree right
evaluateExpressionTree (BinaryNode EP.SUBSTRACTION   left right) = evaluateExpressionTree left -  evaluateExpressionTree right
evaluateExpressionTree (BinaryNode EP.MULTIPLICATION left right) = evaluateExpressionTree left *  evaluateExpressionTree right
evaluateExpressionTree (BinaryNode EP.DIVISION       left right) = evaluateExpressionTreeDivision (evaluateExpressionTree left) (evaluateExpressionTree right)
evaluateExpressionTree (BinaryNode EP.POWER          left right) = evaluateExpressionTree left ** evaluateExpressionTree right

evaluateExpressionTreeDivision :: OperandType -> OperandType -> OperandType
evaluateExpressionTreeDivision left right
  | right == 0  = throw $ ExpressionEvaluationError "Forbidden operation: Division by zero"
  | otherwise   = left / right
