module Error             ( Error(..) ) where

import Control.Exception ( Exception )

data Error = HelpError
           | TooManyArgumentsError
           | TooFewArgumentsError
           deriving (Show, Eq)
instance Exception Error
