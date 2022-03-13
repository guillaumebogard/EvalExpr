--
-- EvalExpr
-- File description:
-- Expression.Lexer.Exception
--

module Expression.Lexer.Exception      ( ExpressionLexerException(..) ) where

import qualified GHC.Exception as GHCE ( Exception )


newtype ExpressionLexerException = ExpressionLexerException String
    deriving Eq


instance GHCE.Exception ExpressionLexerException

instance Show           ExpressionLexerException where
    show (ExpressionLexerException value) = "Expression Lexer Exception: " ++ value ++ "."
