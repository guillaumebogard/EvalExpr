--
-- EvalExpr
-- File description:
-- Argument.ParserSpec
--
{-# LANGUAGE InstanceSigs #-}

module           Argument.ParserSpec              ( spec ) where

import           Test.Hspec                       ( Spec
                                                  , it
                                                  , shouldThrow
                                                  )
import           Control.Exception                ( evaluate )

import qualified Argument.Parser           as AP  ( Expression( Expression )
                                                  , parse
                                                  )

import qualified Argument.Parser.Exception as APE ( ArgumentParserException( ArgumentParserHelpException
                                                                           , ArgumentParserException
                                                                           )
                                                  )


newtype TestExpression = TestExpression AP.Expression


instance Eq TestExpression where
    (==) :: TestExpression -> TestExpression -> Bool
    (TestExpression (AP.Expression left)) == (TestExpression (AP.Expression right)) = left == right

spec :: Spec
spec = do
    it "No argument given" $
        evaluate (AP.parse [])
            `shouldThrow` (== APE.ArgumentParserException "Requires at least one expression, retry with '-h'")
    it "Multiple expressions given" $
        evaluate (AP.parse ["1+1", "2+2"])
            `shouldThrow` (== APE.ArgumentParserException "Can only take one expression")
    it "Single short help option" $
        evaluate (AP.parse ["-h"])
            `shouldThrow` (== APE.ArgumentParserHelpException)
    it "Single long help option" $
        evaluate (AP.parse ["--help"])
            `shouldThrow` (== APE.ArgumentParserHelpException)
    it "Multiple short & long help options" $
        evaluate (AP.parse ["-h", "--help"])
            `shouldThrow` (== APE.ArgumentParserHelpException)
    it "Single expression given" $
        TestExpression (AP.parse ["1+2"])
            == TestExpression (AP.Expression "1+2")
    it "Single expression given with short help option" $
        evaluate (AP.parse ["1+2", "-h"])
            `shouldThrow` (== APE.ArgumentParserHelpException)
    it "Single expression given with unknown option" $
        evaluate (AP.parse ["1+2", "-a"])
            `shouldThrow` (== APE.ArgumentParserException "Unknown option: '-a'")
