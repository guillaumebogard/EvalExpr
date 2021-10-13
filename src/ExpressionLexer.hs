module ExpressionLexer   ( Token(..)
                         , OperandType
                         , tokenizeExpression
                         ) where

import Control.Exception ( throw )
import Data.Char         ( isDigit )
import Text.Read         ( readMaybe )

import Error             ( Error(..) )

type OperandType = Double

data Token = PLUS
           | MINUS
           | TIMES
           | DIVIDE
           | POWER
           | OPENED_PARENTHESIS
           | CLOSED_PARENTHESIS
           | OPERAND OperandType
           deriving (Show, Eq)

tokenizeExpression :: String -> [Token]
tokenizeExpression []        = []
tokenizeExpression (' ':xs)  = tokenizeExpression xs
tokenizeExpression ('\t':xs) = tokenizeExpression xs
tokenizeExpression ('+':xs)  = PLUS               : tokenizeExpression xs
tokenizeExpression ('-':xs)  = MINUS              : tokenizeExpression xs
tokenizeExpression ('*':xs)  = TIMES              : tokenizeExpression xs
tokenizeExpression ('/':xs)  = DIVIDE             : tokenizeExpression xs
tokenizeExpression ('^':xs)  = POWER              : tokenizeExpression xs
tokenizeExpression ('(':xs)  = OPENED_PARENTHESIS : tokenizeExpression xs
tokenizeExpression (')':xs)  = CLOSED_PARENTHESIS : tokenizeExpression xs
tokenizeExpression expr      = let (value, xs) = tokenizeOperand expr in (OPERAND value : tokenizeExpression xs)

tokenizeOperand :: String -> (OperandType, String)
tokenizeOperand = checkOperand . convertOperand . getOperand

checkOperand :: (Maybe OperandType, String) -> (OperandType, String)
checkOperand (Just value, expr) = (value, expr)
checkOperand _                  = throw InvalidExpressionError

convertOperand :: (String, String) -> (Maybe OperandType, String)
convertOperand (str_value, rest) = (readMaybe str_value :: Maybe OperandType, rest)

getOperand :: String -> (String, String)
getOperand expr = getOperand' expr ""

getOperand' :: String -> String -> (String, String)
getOperand' []            number = (number, [])
getOperand' expr@(' ':_)  number = (number, expr)
getOperand' expr@('\t':_) number = (number, expr)
getOperand' expr@('+':_)  number = (number, expr)
getOperand' expr@('-':_)  number = (number, expr)
getOperand' expr@('*':_)  number = (number, expr)
getOperand' expr@('/':_)  number = (number, expr)
getOperand' expr@('^':_)  number = (number, expr)
getOperand' expr@('(':_)  number = (number, expr)
getOperand' expr@(')':_)  number = (number, expr)
getOperand' (x:xs)        number = let (number', xs') = getOperand' xs number in (x:number', xs')

--tokenizeOperand :: String -> (OperandType, String)
--tokenizeOperand = checkOperand . convertOperand . getOperand
--
--checkOperand :: (Maybe OperandType, String) -> (OperandType, String)
--checkOperand (Just value, expr) = (value, expr)
--checkOperand _                  = throw InvalidExpressionError
--
--convertOperand :: (String, String) -> (Maybe OperandType, String)
--convertOperand (str_value, rest) = (readMaybe str_value :: Maybe OperandType, rest)
--
--getOperand :: String -> (String, String)
--getOperand = getOperand' []
--
--getOperand' :: String -> String -> (String, String)
--getOperand' str_value []       = (reverse str_value, [])
--getOperand' str_value ('.':xs) = getOperand' ('.':str_value) xs
--getOperand' str_value rest@(x:xs)
--    | isDigit x                = getOperand' (x:str_value) xs
--    | otherwise                = (reverse str_value, rest)
