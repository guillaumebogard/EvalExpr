module Main              ( main ) where

import Test.Hspec        ( hspec
                         , Spec
                         , describe )

import ArgumentLexerSpec  ( spec )
import ArgumentParserSpec ( spec )

spec :: Spec
spec = do
    describe "ArgumentLexer"  ArgumentLexerSpec.spec
    describe "ArgumentParser" ArgumentParserSpec.spec

main :: IO ()
main = hspec Main.spec
