module Error             ( Error(..) ) where

import Control.Exception ( Exception )

newtype Error = Error String deriving (Show, Eq)
instance Exception Error
