module Main              ( main ) where

import Test.Hspec        ( hspec
                         , Spec
                         , describe )

import ArgumentLexerSpec ( spec )

spec :: Spec
spec = do
    describe "ArgumentLexer" ArgumentLexerSpec.spec

main :: IO ()
main = hspec Main.spec
