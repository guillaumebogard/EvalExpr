--
-- EvalExpr
-- File description:
-- Spec
--

module Main                                        ( main ) where

import qualified Test.Hspec                 as TH  ( Spec
                                                   , hspec
                                                   , describe
                                                   )

import qualified Argument.Lexer.Spec        as ALS ( spec )
import qualified Argument.Parser.Spec       as APS ( spec )
import qualified Expression.Lexer.Spec      as ELS ( spec )
import qualified Expression.Parser.Spec     as EPS ( spec )
import qualified Expression.Evaluation.Spec as EES ( spec )

main :: IO ()
main = TH.hspec Main.spec

spec :: TH.Spec
spec = do
    TH.describe "Argument.Lexer"        ALS.spec
    TH.describe "Argument.Parser"       APS.spec
    TH.describe "Expression.Lexer"      ELS.spec
    TH.describe "Expression.Parser"     EPS.spec
    TH.describe "Expression.Evaluation" EES.spec
