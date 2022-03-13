--
-- EvalExpr
-- File description:
-- Argument.ParserSpec
--

module Argument.ParserSpec                        ( spec ) where

import qualified Test.Hspec                as TH  ( Spec
                                                  , it
                                                  , shouldThrow
                                                  )
import qualified Control.Exception         as CE  ( evaluate )

import qualified Argument.Parser           as AP  ( Expression( Expression )
                                                  , parse
                                                  )

import qualified Argument.Parser.Exception as APE ( ArgumentParserException( ArgumentParserHelpException
                                                                           , ArgumentParserException
                                                                           )
                                                  )


newtype TestExpression = TestExpression AP.Expression


instance Eq TestExpression where
    (TestExpression (AP.Expression left)) == (TestExpression (AP.Expression right)) = left == right

spec :: TH.Spec
spec = do
    TH.it "No argument given" $
        CE.evaluate (AP.parse [])
            `TH.shouldThrow` (== APE.ArgumentParserException "Requires at least one expression, retry with '-h'")
    TH.it "Multiple expressions given" $
        CE.evaluate (AP.parse ["1+1", "2+2"])
            `TH.shouldThrow` (== APE.ArgumentParserException "Can only take one expression")
    TH.it "Single short help option" $
        CE.evaluate (AP.parse ["-h"])
            `TH.shouldThrow` (== APE.ArgumentParserHelpException)
    TH.it "Single long help option" $
        CE.evaluate (AP.parse ["--help"])
            `TH.shouldThrow` (== APE.ArgumentParserHelpException)
    TH.it "Multiple short & long help options" $
        CE.evaluate (AP.parse ["-h", "--help"])
            `TH.shouldThrow` (== APE.ArgumentParserHelpException)
    TH.it "Single expression given" $
        TestExpression (AP.parse ["1+2"])
            == TestExpression (AP.Expression "1+2")
    TH.it "Single expression given with short help option" $
        CE.evaluate (AP.parse ["1+2", "-h"])
            `TH.shouldThrow` (== APE.ArgumentParserHelpException)
    TH.it "Single expression given with unknown option" $
        CE.evaluate (AP.parse ["1+2", "-a"])
            `TH.shouldThrow` (== APE.ArgumentParserException "Unknown option: '-a'")
