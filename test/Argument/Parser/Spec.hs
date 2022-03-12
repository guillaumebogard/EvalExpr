--
-- EvalExpr
-- File description:
-- Argument.Parser.Spec
--

module Argument.Parser.Spec                       ( spec ) where

import qualified Test.Hspec                as TH  ( Spec
                                                  , it
                                                  , shouldThrow
                                                  )
import Control.Exception                   as CE  ( evaluate )

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
    TH.it "Single short help option" $
        CE.evaluate (AP.parse ["-h"])
            `TH.shouldThrow` (== APE.ArgumentParserHelpException)
