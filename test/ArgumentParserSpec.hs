module ArgumentParserSpec where

import Test.Hspec           ( Spec
                            , it )

import ArgumentParser as AP ( Expression(..)
                            , parseArgs )

spec :: Spec
spec = do
    it "Simple expression" $ do
        let (Expression expr) = parseArgs ["10+20"] in expr == "10+20"
    it "Complex expression" $ do
        let (Expression expr) = parseArgs ["(10+20)^2+2"] in expr == "(10+20)^2+2"
