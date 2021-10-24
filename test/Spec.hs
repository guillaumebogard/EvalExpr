module Main                 ( main ) where

import Test.Hspec           ( hspec
                            , Spec
                            , describe )

import IntegrationSpec      ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "Integration"      IntegrationSpec.spec
