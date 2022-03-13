--
-- EvalExpr
-- File description:
-- Expression.Parser.Exception
--

module Expression.Parser.Exception ( ExpressionParserException(..) ) where

import GHC.Exception               ( Exception )


newtype ExpressionParserException = ExpressionParserException String
    deriving Eq


instance Exception ExpressionParserException

instance Show      ExpressionParserException where
    show (ExpressionParserException value) = "Expression Parser Exception: " ++ value ++ "."
