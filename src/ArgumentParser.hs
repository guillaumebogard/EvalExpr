module ArgumentParser ( Conf(..)
                      , parseArgs
                      ) where

import Error          ( Error(..) )

newtype Conf = Conf String deriving (Show)

parseArgs :: [String] -> Either Error Conf
parseArgs _ = Right $ Conf "a"
