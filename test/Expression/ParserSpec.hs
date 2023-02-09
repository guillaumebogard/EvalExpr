--
-- EvalExpr
-- File description:
-- Expression.ParserSpec
--
{-# LANGUAGE InstanceSigs #-}

module           Expression.ParserSpec              ( spec ) where

import           Test.Hspec                         ( Spec
                                                    , it
                                                    , shouldThrow
                                                    )
import           Control.Exception                  ( evaluate )

import qualified Argument.Parser             as AP  ( Expression( Expression ) )
import qualified Expression.Lexer            as EL  ( Token(..) )
import qualified Expression.Parser           as EP  ( UnaryOperator(..)
                                                    , BinaryOperator(..)
                                                    , ExpressionTree(..)
                                                    , parse
                                                    )

import qualified Expression.Parser.Exception as EPE ( ExpressionParserException( ExpressionParserException ) )


newtype TestUnaryOperator = TestUnaryOperator EP.UnaryOperator


instance Eq TestUnaryOperator where
    (==) :: TestUnaryOperator -> TestUnaryOperator -> Bool
    (TestUnaryOperator EP.Plus ) == (TestUnaryOperator EP.Plus ) = True
    (TestUnaryOperator EP.Minus) == (TestUnaryOperator EP.Minus) = True
    _                            == _                            = False


newtype TestBinaryOperator = TestBinaryOperator EP.BinaryOperator


instance Eq TestBinaryOperator where
    (==) :: TestBinaryOperator -> TestBinaryOperator -> Bool
    (TestBinaryOperator EP.Addition       ) == (TestBinaryOperator EP.Addition       ) = True
    (TestBinaryOperator EP.Substraction   ) == (TestBinaryOperator EP.Substraction   ) = True
    (TestBinaryOperator EP.Multiplication ) == (TestBinaryOperator EP.Multiplication ) = True
    (TestBinaryOperator EP.Division       ) == (TestBinaryOperator EP.Division       ) = True
    (TestBinaryOperator EP.Power          ) == (TestBinaryOperator EP.Power          ) = True
    _                                       == _                                       = False


newtype TestExpressionTree = TestExpressionTree EP.ExpressionTree


instance Eq TestExpressionTree where
    (==) :: TestExpressionTree -> TestExpressionTree -> Bool
    (TestExpressionTree (EP.Leaf          left                               )) == (TestExpressionTree (EP.Leaf          right                              )) = left == right
    (TestExpressionTree (EP.ProtectedNode subtree1                           )) == (TestExpressionTree (EP.ProtectedNode subtree2                           )) = TestExpressionTree subtree1 == TestExpressionTree subtree2
    (TestExpressionTree (EP.UnaryNode     op1      subtree1                  )) == (TestExpressionTree (EP.UnaryNode     op2      subtree2                  )) = TestUnaryOperator op1 == TestUnaryOperator op2 && TestExpressionTree subtree1 == TestExpressionTree subtree2
    (TestExpressionTree (EP.BinaryNode    op1      subtreeLeft1 subtreeRight1)) == (TestExpressionTree (EP.BinaryNode    op2      subtreeLeft2 subtreeRight2)) = TestBinaryOperator op1 == TestBinaryOperator op2 && TestExpressionTree subtreeLeft1 == TestExpressionTree subtreeLeft2 && TestExpressionTree subtreeRight1 == TestExpressionTree subtreeRight2
    _                                                                           == _                                                                           = False


spec :: Spec
spec = do
    it "Valid expression: \"1+1\"" $
        TestExpressionTree (EP.parse (AP.Expression "1+1"))
            == TestExpressionTree (
                EP.BinaryNode
                    EP.Addition
                    (EP.Leaf 1)
                    (EP.Leaf 1)
            )
    it "Valid expression: \"2*3+1\"" $
        TestExpressionTree (EP.parse (AP.Expression "2*3+1"))
            == TestExpressionTree (
                EP.BinaryNode
                    EP.Addition
                    (EP.BinaryNode
                        EP.Multiplication
                        (EP.Leaf 2)
                        (EP.Leaf 3)
                    )
                    (EP.Leaf 1)
            )
    it "Valid expression: \"2*(3+1)\"" $
        TestExpressionTree (EP.parse (AP.Expression "2*(3+1)"))
            == TestExpressionTree (
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
            )
    it "Valid expression: \"(2+3)*1\"" $
        TestExpressionTree (EP.parse (AP.Expression "(2+3)*1"))
            == TestExpressionTree (
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
            )
    it "Valid expression: \"(2+3)*(1+10/(1+1))\"" $
        TestExpressionTree (EP.parse (AP.Expression "(2+3)*(1+10/(1+1))"))
            == TestExpressionTree (
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
            )
    it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1\"" $
        TestExpressionTree (EP.parse (AP.Expression "(2+3)*(1+10/(1.1+1))^2.1"))
            == TestExpressionTree (
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
            )
    it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1/1\"" $
        TestExpressionTree (EP.parse (AP.Expression "(2+3)*(1+10/(1.1+1))^2.1/1"))
            == TestExpressionTree (
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
            )
    it "Valid expression: \"++1\"" $
        TestExpressionTree (EP.parse (AP.Expression "++1"))
            == TestExpressionTree (
                EP.UnaryNode
                    EP.Plus
                    (EP.UnaryNode
                        EP.Plus
                        (EP.Leaf 1)
                    )
            )
    it "Invalid expression: \"(1+1\"" $
        evaluate (EP.parse (AP.Expression "(1+1"))
            `shouldThrow` (== EPE.ExpressionParserException "Mismatched parentheses")
