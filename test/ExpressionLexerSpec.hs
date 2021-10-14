module ExpressionLexerSpec ( spec ) where

import Test.Hspec          ( Spec
                           , it
                           , shouldBe
                           , shouldThrow
                           , anyException )

import Control.Exception   ( evaluate )

import ExpressionLexer     ( Token(..)
                           , OperandType(..)
                           , tokenizeExpression )
import Error               ( Error(..) )

spec :: Spec
spec = do
    it "[tokenizeExpression]: Correct expression, decimal and long operands" $ tokenizeExpression "(12+323.1)*32"       `shouldBe`    [OPENED_PARENTHESIS, OPERAND 12, PLUS, OPERAND 323.1, CLOSED_PARENTHESIS, TIMES, OPERAND 32]
    it "[tokenizeExpression]: Correct expression, one character operands"    $ tokenizeExpression "(1+3)*3"             `shouldBe`    [OPENED_PARENTHESIS, OPERAND  1, PLUS, OPERAND     3, CLOSED_PARENTHESIS, TIMES, OPERAND  3]
    --it "[tokenizeExpression]: Incorrect expression"                          $ evaluate (tokenizeExpression "(1+3)*3a") `shouldThrow` (== InvalidExpressionError)
