--
-- EvalExpr
-- File description:
-- Argument.Parser.Exception
--

{-# LANGUAGE InstanceSigs #-}

module Argument.Parser.Exception ( ArgumentParserException(..) ) where

import Control.Exception         ( Exception )


data ArgumentParserException = ArgumentParserHelpException
                             | ArgumentParserException     String
    deriving Eq

instance Exception ArgumentParserException

instance Show      ArgumentParserException where
    show :: ArgumentParserException -> String
    show ArgumentParserHelpException     = usage
    show (ArgumentParserException value) = "Argument Parser Exception: " ++ value ++ "."


usage :: String
usage = "Usage: ./funEvalExpr expression\n"                        ++
        "Description:\n"                                           ++
        "\tAn evaluator for mathematical expressions.\n"           ++
        "Arguments:\n"                                             ++
        "\texpression\tA mathematical expression to be evaluated." ++
        "\n"                                                       ++
        "Options:\n"                                               ++
        "\t--help -h\tDisplay this information."
