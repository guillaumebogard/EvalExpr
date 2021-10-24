module ExpressionParserSpec   ( spec ) where

import Test.Hspec             ( Spec
                              , it )

import ArgumentParser         ( Expression(..) )
import ExpressionParser as EP ( UnaryOperator(..)
                              , BinaryOperator(..)
                              , ExpressionTree(..)
                              , parseExpression )

newtype TestUnaryOperator = TestUnaryOperator UnaryOperator
instance Eq TestUnaryOperator where
    (TestUnaryOperator EP.PLUS)  == (TestUnaryOperator EP.PLUS)  = True
    (TestUnaryOperator EP.MINUS) == (TestUnaryOperator EP.MINUS) = True
    _                            == _                            = False

newtype TestBinaryOperator = TestBinaryOperator BinaryOperator
instance Eq TestBinaryOperator where
    (TestBinaryOperator EP.ADDITION)       == (TestBinaryOperator EP.ADDITION)       = True
    (TestBinaryOperator EP.SUBSTRACTION)   == (TestBinaryOperator EP.SUBSTRACTION)   = True
    (TestBinaryOperator EP.MULTIPLICATION) == (TestBinaryOperator EP.MULTIPLICATION) = True
    (TestBinaryOperator EP.DIVISION)       == (TestBinaryOperator EP.DIVISION)       = True
    (TestBinaryOperator EP.POWER)          == (TestBinaryOperator EP.POWER)          = True
    _                                      == _                                      = False

newtype TestExpressionTree = TestExpressionTree ExpressionTree
instance Eq TestExpressionTree where
    (TestExpressionTree (EP.Leaf left))                   == (TestExpressionTree (Leaf right))                     = left == right
    (TestExpressionTree (EP.ProtectedNode left))          == (TestExpressionTree (ProtectedNode right))            = TestExpressionTree left == TestExpressionTree right
    (TestExpressionTree (EP.UnaryNode op1 left))          == (TestExpressionTree (EP.UnaryNode op2 right))         = TestUnaryOperator op1 == TestUnaryOperator op2 && TestExpressionTree left == TestExpressionTree right
    (TestExpressionTree (EP.BinaryNode op1 left1 right1)) == (TestExpressionTree (EP.BinaryNode op2 left2 right2)) = TestBinaryOperator op1 == TestBinaryOperator op2 && TestExpressionTree left1 == TestExpressionTree left2 && TestExpressionTree right1 == TestExpressionTree right2
    _                                                     ==                                                     _ = False

spec :: Spec
spec = do
    it "Simple expression" $ do
        TestExpressionTree (parseExpression (Expression "1+2")) == TestExpressionTree (EP.BinaryNode EP.ADDITION (EP.Leaf 1) (EP.Leaf 2))
    it "Multiple unary minus" $ do
        TestExpressionTree (parseExpression (Expression "---1+2")) == TestExpressionTree (EP.BinaryNode EP.ADDITION (EP.UnaryNode EP.MINUS (UnaryNode EP.MINUS (UnaryNode EP.MINUS (EP.Leaf 1)))) (EP.Leaf 2))
    it "Multiple unary plus & minus" $ do
        TestExpressionTree (parseExpression (Expression "-+-1+2")) == TestExpressionTree (EP.BinaryNode EP.ADDITION (EP.UnaryNode EP.MINUS (UnaryNode EP.PLUS (UnaryNode EP.MINUS (EP.Leaf 1)))) (EP.Leaf 2))
    it "Basic priorities" $ do
        TestExpressionTree (parseExpression (Expression "1+2*3")) == TestExpressionTree (EP.BinaryNode EP.ADDITION (EP.Leaf 1) (EP.BinaryNode EP.MULTIPLICATION (EP.Leaf 2) (EP.Leaf 3)))
    it "Advanced priorities" $ do
        TestExpressionTree (parseExpression (Expression "(1+2)*3^2")) == TestExpressionTree (EP.BinaryNode EP.MULTIPLICATION (EP.ProtectedNode (EP.BinaryNode EP.ADDITION (EP.Leaf 1) (EP.Leaf 2))) (EP.BinaryNode EP.POWER (EP.Leaf 3) (EP.Leaf 2)))
