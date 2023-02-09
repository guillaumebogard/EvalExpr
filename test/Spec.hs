--
-- EvalExpr
-- File description:
-- Spec
--

module           Main                             ( main ) where

import           Test.Hspec                       ( Spec
                                                  , hspec
                                                  , describe
                                                  )

import qualified Argument.LexerSpec        as ALS ( spec )
import qualified Argument.ParserSpec       as APS ( spec )
import qualified Expression.LexerSpec      as ELS ( spec )
import qualified Expression.ParserSpec     as EPS ( spec )
import qualified Expression.EvaluationSpec as EES ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "Argument.Lexer"        ALS.spec
    describe "Argument.Parser"       APS.spec
    describe "Expression.Lexer"      ELS.spec
    describe "Expression.Parser"     EPS.spec
    describe "Expression.Evaluation" EES.spec
