module Error             ( Error(..) ) where

import Control.Exception ( Exception )

data Error = HelpError
           | ArgumentParserError String
           | InvalidExpressionError String
           | ExpressionEvaluationError String
           deriving Show
instance Exception Error
