--
-- EvalExpr
-- File description:
-- Expression.Lexer.Exception
--

module Expression.Lexer.Exception ( ExpressionLexerException(..) ) where

import GHC.Exception              ( Exception )


newtype ExpressionLexerException = ExpressionLexerException String
    deriving Eq


instance Exception ExpressionLexerException

instance Show      ExpressionLexerException where
    show (ExpressionLexerException value) = "Expression Lexer Exception: " ++ value ++ "."
