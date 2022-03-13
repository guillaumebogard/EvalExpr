--
-- EvalExpr
-- File description:
-- Expression.Evaluation.Exception
--

module Expression.Evaluation.Exception ( ExpressionEvaluationException(..) ) where

import qualified GHC.Exception as GHCE ( Exception )


newtype ExpressionEvaluationException = ExpressionEvaluationException String
    deriving Eq


instance GHCE.Exception ExpressionEvaluationException

instance Show           ExpressionEvaluationException where
    show (ExpressionEvaluationException value) = "Expression Evaluation Exception: " ++ value ++ "."
