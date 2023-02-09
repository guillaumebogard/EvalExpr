--
-- EvalExpr
-- File description:
-- Expression.Display
--

module Expression.Display                    ( display ) where

import           Text.Printf                 ( printf )

import qualified Expression.Evaluation as EE ( EvaluationResult( EvaluationResult ) )


display :: EE.EvaluationResult -> IO ()
display (EE.EvaluationResult value) = printf "%.2f\n" value
