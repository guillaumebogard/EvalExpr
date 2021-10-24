module ExpressionParser      ( UnaryOperator(..)
                             , BinaryOperator(..)
                             , ExpressionTree(..)
                             , parseExpression
                             ) where

import GHC.Exception         ( throw )

import Error                 ( Error(InvalidExpressionError) )
import ArgumentParser        ( Expression(..) )
import ExpressionLexer as EL ( Token(..)
                             , OperandType
                             , tokenizeExpression )

data UnaryOperator  = PLUS
                    | MINUS

data BinaryOperator = ADDITION
                    | SUBSTRACTION
                    | MULTIPLICATION
                    | DIVISION
                    | POWER

data ExpressionTree = UnaryNode UnaryOperator ExpressionTree
                    | BinaryNode BinaryOperator ExpressionTree ExpressionTree
                    | ProtectedNode ExpressionTree
                    | Leaf OperandType

parseExpression :: Expression -> ExpressionTree
parseExpression = parseTokenizedExpression . tokenizeExpression

parseTokenizedExpression :: [Token] -> ExpressionTree
parseTokenizedExpression = uncurry loopOverTokens . endTree

loopOverTokens :: [Token] -> ExpressionTree -> ExpressionTree
loopOverTokens [] tree                                  = tree
loopOverTokens (x:xs)   node@(Leaf _)                   = binaryNodeAndLoop (endTree xs) x node
loopOverTokens (x:xs)   node@(UnaryNode _ _)            = binaryNodeAndLoop (endTree xs) x node
loopOverTokens (x:xs)   node@(ProtectedNode _)          = binaryNodeAndLoop (endTree xs) x node
loopOverTokens (op:xs) node@BinaryNode {}          = let (rest, new) = endTree xs in loopOverTokens rest $ placeTokenInTree node op new

binaryNodeAndLoop :: ([Token], ExpressionTree) -> Token -> ExpressionTree -> ExpressionTree
binaryNodeAndLoop (rest, new) x node = loopOverTokens rest $ BinaryNode (tokenToOp x) node new

placeTokenInTree :: ExpressionTree -> Token -> ExpressionTree -> ExpressionTree
placeTokenInTree base@(BinaryNode op left right) newOp tree
  | isLessPrio (tokenToOp newOp) op                = BinaryNode (tokenToOp newOp) base tree
  | otherwise                                      = BinaryNode op left $ placeTokenInTree right newOp tree
placeTokenInTree base@(Leaf _) newOp tree          = BinaryNode (tokenToOp newOp) base tree
placeTokenInTree base@(UnaryNode _ _) newOp tree   = BinaryNode (tokenToOp newOp) base tree
placeTokenInTree base@(ProtectedNode _) newOp tree = BinaryNode (tokenToOp newOp) base tree

endTree :: [Token] -> ([Token], ExpressionTree)
endTree (EL.OPERAND operand:xs)     = (xs, Leaf operand)
endTree (EL.SUBSTRACTION:xs)        = makeEndTreeTuple (endTree xs) MINUS
endTree (EL.ADDITION:xs)            = makeEndTreeTuple (endTree xs) PLUS
endTree list@(OPENED_PARENTHESIS:_) = let (expression, rest) = unwrapExpression list in (rest, ProtectedNode $ parseTokenizedExpression expression)
endTree _                           = throw $ InvalidExpressionError "An operator is missing (an) operand(s)"

makeEndTreeTuple :: ([Token], ExpressionTree) -> UnaryOperator -> ([Token], ExpressionTree)
makeEndTreeTuple (rest, tree) MINUS = (rest, UnaryNode MINUS tree)
makeEndTreeTuple (rest, tree) PLUS  = (rest, UnaryNode PLUS tree)

tokenToOp :: Token -> BinaryOperator
tokenToOp EL.ADDITION       = ExpressionParser.ADDITION
tokenToOp EL.SUBSTRACTION   = ExpressionParser.SUBSTRACTION
tokenToOp EL.MULTIPLICATION = ExpressionParser.MULTIPLICATION
tokenToOp EL.DIVISION       = ExpressionParser.DIVISION
tokenToOp EL.POWER          = ExpressionParser.POWER
tokenToOp _                 = throw $ InvalidExpressionError "Invalid syntax"

isLessPrio :: BinaryOperator -> BinaryOperator -> Bool
isLessPrio left right
  | getPrio left <= getPrio right   = True
  | otherwise                       = False

getPrio :: BinaryOperator -> Int
getPrio ExpressionParser.ADDITION       = 1
getPrio ExpressionParser.SUBSTRACTION   = 1
getPrio ExpressionParser.MULTIPLICATION = 2
getPrio ExpressionParser.DIVISION       = 2
getPrio ExpressionParser.POWER          = 3

unwrapExpression :: [Token] -> ([Token], [Token])
unwrapExpression (OPENED_PARENTHESIS:xs)  = postUnwrapExpression ([], xs) 0
unwrapExpression []                     = throw $ InvalidExpressionError "empty parenthesis are not allowed."
unwrapExpression _                      = throw $ InvalidExpressionError "Invalid syntax (unwrapExpression)"

postUnwrapExpression :: ([Token], [Token]) -> Int -> ([Token], [Token])
postUnwrapExpression (left, CLOSED_PARENTHESIS:xs) 0       = (reverse left, xs)
postUnwrapExpression (left, x@CLOSED_PARENTHESIS:xs) count   = postUnwrapExpression (x:left, xs) $ count - 1
postUnwrapExpression (left, x@OPENED_PARENTHESIS:xs) count    = postUnwrapExpression (x:left, xs) $ count + 1
postUnwrapExpression (left, x:xs) count                     = postUnwrapExpression (x:left, xs) count
postUnwrapExpression (_, _) _                               = throw $ InvalidExpressionError "invalidSyntax (postUnwrapExpression)"
