--
-- EvalExpr
-- File description:
-- Expression.Evaluation.Exception
--

module Expression.Evaluation.Exception ( ExpressionEvaluationException(..) ) where

import GHC.Exception                   ( Exception )


newtype ExpressionEvaluationException = ExpressionEvaluationException String
    deriving Eq


instance Exception ExpressionEvaluationException

instance Show      ExpressionEvaluationException where
    show (ExpressionEvaluationException value) = "Expression Evaluation Exception: " ++ value ++ "."
