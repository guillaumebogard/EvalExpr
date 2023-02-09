--
-- EvalExpr
-- File description:
-- Expression.Parser.Exception
--
{-# LANGUAGE InstanceSigs #-}

module Expression.Parser.Exception ( ExpressionParserException(..) ) where

import GHC.Exception               ( Exception )


newtype ExpressionParserException = ExpressionParserException String
    deriving Eq


instance Exception ExpressionParserException

instance Show      ExpressionParserException where
    show :: ExpressionParserException -> String
    show (ExpressionParserException value) = "Expression Parser Exception: " ++ value ++ "."
