--
-- EvalExpr
-- File description:
-- Expression.Lexer.Exception
--

{-# LANGUAGE InstanceSigs #-}

module Expression.Lexer.Exception ( ExpressionLexerException(..) ) where

import Control.Exception          ( Exception )


newtype ExpressionLexerException = ExpressionLexerException String
    deriving Eq

instance Exception ExpressionLexerException

instance Show      ExpressionLexerException where
    show :: ExpressionLexerException -> String
    show (ExpressionLexerException value) = "Expression Lexer Exception: " ++ value ++ "."
