--
-- EvalExpr
-- File description:
-- Expression.Lexer.Spec
--

module Expression.Lexer.Spec                       ( spec ) where

import qualified Test.Hspec                 as TH  ( Spec
                                                   , it
                                                   , shouldThrow
                                                   )

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
    TH.it "Simple valid expression: \"1+1\"" $
        map TestToken (EL.tokenize (AP.Expression "1+1"))
            == map TestToken [
                EL.Operand 1
              , EL.Addition
              , EL.Operand 1
            ]
