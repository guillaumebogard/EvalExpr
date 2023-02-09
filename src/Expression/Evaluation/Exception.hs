--
-- EvalExpr
-- File description:
-- Expression.Evaluation.Exception
--
{-# LANGUAGE InstanceSigs #-}

module Expression.Evaluation.Exception ( ExpressionEvaluationException(..) ) where

import Control.Exception               ( Exception )


newtype ExpressionEvaluationException = ExpressionEvaluationException String
    deriving Eq


instance Exception ExpressionEvaluationException

instance Show      ExpressionEvaluationException where
    show :: ExpressionEvaluationException -> String
    show (ExpressionEvaluationException value) = "Expression Evaluation Exception: " ++ value ++ "."
