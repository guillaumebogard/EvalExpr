module ExpressionLexerSpec   ( spec ) where

import Test.Hspec            ( Spec
                             , it )

import ArgumentParser        ( Expression(..) )
import ExpressionLexer as EL ( Token(..)
                             , OperandType
                             , tokenizeExpression )

newtype TestToken = TestToken EL.Token
instance Eq TestToken where
    (TestToken EL.ADDITION)           == (TestToken EL.ADDITION)           = True
    (TestToken EL.SUBSTRACTION)       == (TestToken EL.SUBSTRACTION)       = True
    (TestToken EL.MULTIPLICATION)     == (TestToken EL.MULTIPLICATION)     = True
    (TestToken EL.DIVISION)           == (TestToken EL.DIVISION)           = True
    (TestToken EL.POWER)              == (TestToken EL.POWER)              = True
    (TestToken EL.OPENED_PARENTHESIS) == (TestToken EL.OPENED_PARENTHESIS) = True
    (TestToken EL.CLOSED_PARENTHESIS) == (TestToken EL.CLOSED_PARENTHESIS) = True
    (TestToken (EL.OPERAND left))     == (TestToken (EL.OPERAND right))    = left == right
    _                                 == _                                 = False

spec :: Spec
spec = do
    it "Simple expression" $ do
        map TestToken (tokenizeExpression (Expression "1+3"))
            == [TestToken (EL.OPERAND 1), TestToken EL.ADDITION, TestToken (EL.OPERAND 3)]
    it "Complex expression" $ do
        map TestToken (tokenizeExpression (Expression "(1+3)*3.9^(2+1)"))
            == [TestToken EL.OPENED_PARENTHESIS, TestToken (EL.OPERAND 1), TestToken EL.ADDITION, TestToken (EL.OPERAND 3), TestToken EL.CLOSED_PARENTHESIS, TestToken EL.MULTIPLICATION, TestToken (EL.OPERAND 3.9), TestToken EL.POWER, TestToken EL.OPENED_PARENTHESIS, TestToken (EL.OPERAND 2), TestToken EL.ADDITION, TestToken (EL.OPERAND 1), TestToken EL.CLOSED_PARENTHESIS]
