--
-- EvalExpr
-- File description:
-- Expression.Parser.Exception
--

module Expression.Parser.Exception     ( ExpressionParserException(..) ) where

import qualified GHC.Exception as GHCE ( Exception )


newtype ExpressionParserException = ExpressionParserException String
    deriving Eq


instance GHCE.Exception ExpressionParserException

instance Show           ExpressionParserException where
    show (ExpressionParserException value) = "Expression Parser Exception: " ++ value ++ "."
