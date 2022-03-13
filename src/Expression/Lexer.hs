--
-- EvalExpr
-- File description:
-- Expression.Lexer
--

module Expression.Lexer                             ( Token(..)
                                                    , OperandType
                                                    , tokenize
                                                    ) where

import qualified GHC.Exception              as GHCE ( throw )
import qualified Data.Char                  as DC   ( isSpace
                                                    , isDigit
                                                    )

import qualified Argument.Parser            as AP   ( Expression(..) )

import qualified Expression.Lexer.Exception as ELE  ( ExpressionLexerException( ExpressionLexerException ) )

type OperandType = Double

data Token = Operand           OperandType -- 10 | 10.1 | .01
           | Addition                      -- '+'
           | Substraction                  -- '-'
           | Multiplication                -- '*'
           | Division                      -- '/'
           | Power                         -- '^'
           | OpenedParenthesis             -- '('
           | ClosedParenthesis             -- ')'

tokenize :: AP.Expression -> [Token]
tokenize      (AP.Expression      []      ) = []
tokenize      (AP.Expression      ('+':xs)) = Addition          : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('-':xs)) = Substraction      : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('*':xs)) = Multiplication    : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('/':xs)) = Division          : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('^':xs)) = Power             : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('(':xs)) = OpenedParenthesis : tokenize (AP.Expression xs)
tokenize      (AP.Expression      (')':xs)) = ClosedParenthesis : tokenize (AP.Expression xs)
tokenize      (AP.Expression      ('.':xs)) = uncurry addTokenAndContinueTokenize $ tokenizeAfterDot $ AP.Expression xs
tokenize expr@(AP.Expression      (x  :xs))
    | DC.isSpace x                          = tokenize $ AP.Expression xs
    | DC.isDigit x                          = uncurry addTokenAndContinueTokenize $ tokenizeOperand expr
    | otherwise                             = GHCE.throw $ ELE.ExpressionLexerException $ "Unrecognized token: '" ++ x : "'"

addTokenAndContinueTokenize :: Token -> AP.Expression -> [Token]
addTokenAndContinueTokenize token rest = token : tokenize rest

tokenizeAfterDot :: AP.Expression -> (Token, AP.Expression)
tokenizeAfterDot expr@(AP.Expression (x:_))
    | DC.isDigit x                          = tokenizeOperandStartingWithDot expr
    | otherwise                             = GHCE.throw $ ELE.ExpressionLexerException "Incomplete operand starting by '.'"
tokenizeAfterDot      (AP.Expression []   ) = GHCE.throw $ ELE.ExpressionLexerException "Incomplete operand starting by '.'"

tokenizeOperandStartingWithDot :: AP.Expression -> (Token, AP.Expression)
tokenizeOperandStartingWithDot  (AP.Expression expr)   = tokenizeOperandStartingWithDot' $ span DC.isDigit expr

tokenizeOperandStartingWithDot' :: (String, String) -> (Token, AP.Expression)
tokenizeOperandStartingWithDot' (fractionalPart, rest) = (Operand $ read $ "0." ++ fractionalPart, AP.Expression rest)

tokenizeOperand :: AP.Expression -> (Token, AP.Expression)
tokenizeOperand (AP.Expression expr) = tokenizeOperand' $ span DC.isDigit expr

tokenizeOperand' :: (String, String) -> (Token, AP.Expression)
tokenizeOperand' (dp, '.':fp@(x2:_))
    | DC.isDigit x2          = uncurry (tokenizeOperand'' dp) $ span DC.isDigit fp
    | otherwise              = GHCE.throw $ ELE.ExpressionLexerException $ "Incomplete operand ending by dot: '" ++ dp ++ ".'"
tokenizeOperand' (dp, ['.']) = GHCE.throw $ ELE.ExpressionLexerException $ "Incomplete operand ending by dot: '" ++ dp ++ ".'"
tokenizeOperand' (dp, rest ) = (Operand $ read dp, AP.Expression rest)

tokenizeOperand'' :: String -> String -> String -> (Token, AP.Expression)
tokenizeOperand'' dp fp rest = (Operand $ read $ dp ++ '.' : fp, AP.Expression rest)
