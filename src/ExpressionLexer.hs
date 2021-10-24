module ExpressionLexer   ( Token(..)
                         , OperandType
                         , tokenizeExpression
                         ) where

import Control.Exception ( throw )
import Data.Char         ( isSpace )
import Text.Read         ( readMaybe )

import ArgumentParser    ( Expression(..) )

import Error             ( Error(InvalidExpressionError) )

type OperandType = Double

data Token = ADDITION
           | SUBSTRACTION
           | MULTIPLICATION
           | DIVISION
           | POWER
           | OPENED_PARENTHESIS
           | CLOSED_PARENTHESIS
           | OPERAND OperandType

tokenizeExpression :: Expression -> [Token]
tokenizeExpression (Expression [])       = []
tokenizeExpression (Expression ('+':xs)) = ADDITION           : tokenizeExpression (Expression xs)
tokenizeExpression (Expression ('-':xs)) = SUBSTRACTION       : tokenizeExpression (Expression xs)
tokenizeExpression (Expression ('*':xs)) = MULTIPLICATION     : tokenizeExpression (Expression xs)
tokenizeExpression (Expression ('/':xs)) = DIVISION           : tokenizeExpression (Expression xs)
tokenizeExpression (Expression ('^':xs)) = POWER              : tokenizeExpression (Expression xs)
tokenizeExpression (Expression ('(':xs)) = OPENED_PARENTHESIS : tokenizeExpression (Expression xs)
tokenizeExpression (Expression (')':xs)) = CLOSED_PARENTHESIS : tokenizeExpression (Expression xs)
tokenizeExpression expr@(Expression (x:xs))
    | isSpace x = tokenizeExpression $ Expression xs
    | otherwise = let (value, xs') = tokenizeOperand expr in (OPERAND value : tokenizeExpression xs')

tokenizeOperand :: Expression -> (OperandType, Expression)
tokenizeOperand = checkOperand . convertOperand . getOperand

checkOperand :: (Maybe OperandType, Expression) -> (OperandType, Expression)
checkOperand (Just value, expr) = (value, expr)
checkOperand _                  = throw $ InvalidExpressionError "Invalid operand in expression"

convertOperand :: (Expression, Expression) -> (Maybe OperandType, Expression)
convertOperand (Expression str_value, rest) = (readMaybe str_value :: Maybe OperandType, rest)

getOperand :: Expression -> (Expression, Expression)
getOperand expr = getOperand' expr $ Expression ""

getOperand' :: Expression -> Expression -> (Expression, Expression)
<<<<<<< Updated upstream
getOperand' (Expression [])           number = (number, Expression [])
getOperand' expr@(Expression ('+':_)) number = (number, expr)
getOperand' expr@(Expression ('-':_)) number = (number, expr)
getOperand' expr@(Expression ('*':_)) number = (number, expr)
getOperand' expr@(Expression ('/':_)) number = (number, expr)
getOperand' expr@(Expression ('^':_)) number = (number, expr)
getOperand' expr@(Expression ('(':_)) number = (number, expr)
getOperand' expr@(Expression (')':_)) number = (number, expr)
getOperand' expr@(Expression (x:xs))  number
=======
getOperand' (Expression [])            number = (number, Expression [])
getOperand' expr@(Expression ('+':_))  number = (number, expr)
getOperand' expr@(Expression ('-':_))  number = (number, expr)
getOperand' expr@(Expression ('*':_))  number = (number, expr)
getOperand' expr@(Expression ('/':_))  number = (number, expr)
getOperand' expr@(Expression ('^':_))  number = (number, expr)
getOperand' expr@(Expression ('(':_))  number = (number, expr)
getOperand' expr@(Expression (')':_))  number = (number, expr)
getOperand' expr@(Expression (x:xs))   number
>>>>>>> Stashed changes
    | isSpace x = (number, expr)
    | otherwise = let (Expression number', xs') = getOperand' (Expression xs) number in (Expression (x:number'), xs')
