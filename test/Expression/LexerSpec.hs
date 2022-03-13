--
-- EvalExpr
-- File description:
-- Expression.LexerSpec
--

module Expression.LexerSpec                        ( spec ) where

import qualified Test.Hspec                 as TH  ( Spec
                                                   , it
                                                   , shouldThrow
                                                   )
import qualified Control.Exception          as CE  ( evaluate )

import qualified Argument.Parser            as AP  ( Expression( Expression ) )
import qualified Expression.Lexer           as EL  ( Token(..)
                                                   , tokenize
                                                   )

import qualified Expression.Lexer.Exception as ELE ( ExpressionLexerException( ExpressionLexerException ) )


newtype TestToken = TestToken EL.Token


instance Eq TestToken where
    (TestToken (EL.Operand          left)) == (TestToken (EL.Operand          right)) = left == right
    (TestToken EL.Addition               ) == (TestToken EL.Addition                ) = True
    (TestToken EL.Substraction           ) == (TestToken EL.Substraction            ) = True
    (TestToken EL.Multiplication         ) == (TestToken EL.Multiplication          ) = True
    (TestToken EL.Division               ) == (TestToken EL.Division                ) = True
    (TestToken EL.Power                  ) == (TestToken EL.Power                   ) = True
    (TestToken EL.OpenedParenthesis      ) == (TestToken EL.OpenedParenthesis       ) = True
    (TestToken EL.ClosedParenthesis      ) == (TestToken EL.ClosedParenthesis       ) = True
    _                                      == _                                       = False

spec :: TH.Spec
spec = do
    TH.it "Valid expression: \"1+1\"" $
        map TestToken (EL.tokenize (AP.Expression "1+1"))
            == map TestToken [
                EL.Operand 1
              , EL.Addition
              , EL.Operand 1
            ]
    TH.it "Valid expression: \"2*3+1\"" $
        map TestToken (EL.tokenize (AP.Expression "2*3+1"))
            == map TestToken [
                EL.Operand 2
              , EL.Multiplication
              , EL.Operand 3
              , EL.Addition
              , EL.Operand 1
            ]
    TH.it "Valid expression: \"2*(3+1)\"" $
        map TestToken (EL.tokenize (AP.Expression "2*(3+1)"))
            == map TestToken [
                EL.Operand 2
              , EL.Multiplication
              , EL.OpenedParenthesis
                  , EL.Operand 3
                  , EL.Addition
                  , EL.Operand 1
              , EL.ClosedParenthesis
            ]
    TH.it "Valid expression: \"(2+3)*1\"" $
        map TestToken (EL.tokenize (AP.Expression "(2+3)*1"))
            == map TestToken [
                EL.OpenedParenthesis
                  , EL.Operand 2
                  , EL.Addition
                  , EL.Operand 3
              , EL.ClosedParenthesis
              , EL.Multiplication
              , EL.Operand 1
            ]
    TH.it "Valid expression: \"(2+3)*(1+10/(1+1))\"" $
        map TestToken (EL.tokenize (AP.Expression "(2+3)*(1+10/(1+1))"))
            == map TestToken [
                EL.OpenedParenthesis
                  , EL.Operand 2
                  , EL.Addition
                  , EL.Operand 3
              , EL.ClosedParenthesis
              , EL.Multiplication
              , EL.OpenedParenthesis
                  , EL.Operand 1
                  , EL.Addition
                  , EL.Operand 10
                  , EL.Division
                  , EL.OpenedParenthesis
                      , EL.Operand 1
                      , EL.Addition
                      , EL.Operand 1
                  , EL.ClosedParenthesis
              , EL.ClosedParenthesis
            ]
    TH.it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1\"" $
        map TestToken (EL.tokenize (AP.Expression "(2+3)*(1+10/(1.1+1))^2.1"))
            == map TestToken [
                EL.OpenedParenthesis
                  , EL.Operand 2
                  , EL.Addition
                  , EL.Operand 3
              , EL.ClosedParenthesis
              , EL.Multiplication
              , EL.OpenedParenthesis
                  , EL.Operand 1
                  , EL.Addition
                  , EL.Operand 10
                  , EL.Division
                  , EL.OpenedParenthesis
                      , EL.Operand 1.1
                      , EL.Addition
                      , EL.Operand 1
                  , EL.ClosedParenthesis
              , EL.ClosedParenthesis
              , EL.Power
              , EL.Operand 2.1
            ]
    TH.it "Valid expression: \"(2+3)*(1+10/(1.1+1))^2.1/1\"" $
        map TestToken (EL.tokenize (AP.Expression "(2+3)*(1+10/(1.1+1))^2.1/1"))
            == map TestToken [
                EL.OpenedParenthesis
                  , EL.Operand 2
                  , EL.Addition
                  , EL.Operand 3
              , EL.ClosedParenthesis
              , EL.Multiplication
              , EL.OpenedParenthesis
                  , EL.Operand 1
                  , EL.Addition
                  , EL.Operand 10
                  , EL.Division
                  , EL.OpenedParenthesis
                      , EL.Operand 1.1
                      , EL.Addition
                      , EL.Operand 1
                  , EL.ClosedParenthesis
              , EL.ClosedParenthesis
              , EL.Power
              , EL.Operand 2.1
              , EL.Division
              , EL.Operand 1
            ]
    TH.it "Valid expression: \"1   \t\t\t  + \t 1\"" $
        map TestToken (EL.tokenize (AP.Expression "1   \t\t\t  + \t 1"))
            == map TestToken [
                EL.Operand 1
              , EL.Addition
              , EL.Operand 1
            ]
    TH.it "Valid expression: \"1+.1\"" $
        map TestToken (EL.tokenize (AP.Expression "1+.1"))
            == map TestToken [
                EL.Operand 1
              , EL.Addition
              , EL.Operand 0.1
            ]
    TH.it "Valid expression: \".1-.1\"" $
        map TestToken (EL.tokenize (AP.Expression ".1-.1"))
            == map TestToken [
                EL.Operand 0.1
              , EL.Substraction
              , EL.Operand 0.1
            ]
    TH.it "Invalid expression: \"?\"" $
        CE.evaluate (EL.tokenize (AP.Expression "?"))
            `TH.shouldThrow` (== ELE.ExpressionLexerException "Unrecognized token: '?'")
