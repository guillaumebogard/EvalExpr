--
-- EvalExpr
-- File description:
-- Expression.Display
--

module Expression.Display                    ( display ) where

import qualified Text.Printf           as TP ( printf )

import qualified Expression.Evaluation as EE ( EvaluationResult( EvaluationResult ) )


display :: EE.EvaluationResult -> IO ()
display (EE.EvaluationResult value) = TP.printf "%.2f\n" value
