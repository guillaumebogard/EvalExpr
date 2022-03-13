--
-- EvalExpr
-- File description:
-- Expression.Parser
--

module Expression.Parser                             ( UnaryOperator(..)
                                                     , BinaryOperator(..)
                                                     , ExpressionTree(..)
                                                     , parse
                                                     ) where

import qualified GHC.Exception               as GHCE ( throw )

import qualified Argument.Parser             as AP   ( Expression(..) )
import qualified Expression.Lexer            as EL   ( Token(..)
                                                     , OperandType
                                                     , tokenize
                                                     )

import qualified Expression.Parser.Exception as EPE  ( ExpressionParserException( ExpressionParserException ) )


data UnaryOperator  = Plus
                    | Minus

data BinaryOperator = Addition
                    | Substraction
                    | Multiplication
                    | Division
                    | Power

data ExpressionTree = UnaryNode     UnaryOperator  ExpressionTree
                    | BinaryNode    BinaryOperator ExpressionTree ExpressionTree
                    | ProtectedNode ExpressionTree
                    | Leaf          EL.OperandType


parse :: AP.Expression -> ExpressionTree
parse = parseTokens . EL.tokenize

parseTokens :: [EL.Token] -> ExpressionTree
parseTokens = uncurry handleNewExpressionTreeFromLeft . getExpressionTreeFromLeft

getExpressionTreeFromLeft :: [EL.Token] -> ([EL.Token], ExpressionTree)
getExpressionTreeFromLeft (EL.Operand           operand:xs) = (xs, Leaf operand)
getExpressionTreeFromLeft (EL.Addition                 :xs) = wrapTreeAroundUnaryNode     (getExpressionTreeFromLeft xs) Plus
getExpressionTreeFromLeft (EL.Substraction             :xs) = wrapTreeAroundUnaryNode     (getExpressionTreeFromLeft xs) Minus
getExpressionTreeFromLeft (EL.OpenedParenthesis        :xs) = let (subtokens, rest) = getSubtokens xs in
                                                              wrapTreeAroundProtectedNode (rest, parseTokens subtokens)
getExpressionTreeFromLeft (EL.ClosedParenthesis        :_ ) = GHCE.throw $ EPE.ExpressionParserException "Mismatched parentheses"
getExpressionTreeFromLeft _                                 = GHCE.throw $ EPE.ExpressionParserException "An operator is missing its operand(s)"

wrapTreeAroundUnaryNode :: ([EL.Token], ExpressionTree) -> UnaryOperator -> ([EL.Token], ExpressionTree)
wrapTreeAroundUnaryNode     (rest, tree) op = (rest, UnaryNode     op   tree)

wrapTreeAroundProtectedNode :: ([EL.Token], ExpressionTree) -> ([EL.Token], ExpressionTree)
wrapTreeAroundProtectedNode (rest, tree)    = (rest, ProtectedNode tree     )

getSubtokens :: [EL.Token] -> ([EL.Token], [EL.Token])
getSubtokens tokens = getSubtokens' ([], tokens) 0

getSubtokens' :: ([EL.Token], [EL.Token]) -> Int -> ([EL.Token], [EL.Token])
getSubtokens' (subtokens, EL.ClosedParenthesis:xs) 0       = (reverse subtokens, xs)
getSubtokens' (subtokens, EL.ClosedParenthesis:xs) nbMatch = getSubtokens' (EL.ClosedParenthesis:subtokens, xs) $ nbMatch - 1
getSubtokens' (subtokens, EL.OpenedParenthesis:xs) nbMatch = getSubtokens' (EL.OpenedParenthesis:subtokens, xs) $ nbMatch + 1
getSubtokens' (subtokens, x:xs)                    nbMatch = getSubtokens' (x:subtokens, xs) nbMatch
getSubtokens' (_, [])                              _       = GHCE.throw $ EPE.ExpressionParserException "Mismatched parentheses"

handleNewExpressionTreeFromLeft :: [EL.Token] -> ExpressionTree -> ExpressionTree
handleNewExpressionTreeFromLeft (tokenOp:xs) tree@Leaf          {} = let (rest, secondTree) = getExpressionTreeFromLeft xs in handleNewExpressionTreeFromLeft rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
handleNewExpressionTreeFromLeft (tokenOp:xs) tree@ProtectedNode {} = let (rest, secondTree) = getExpressionTreeFromLeft xs in handleNewExpressionTreeFromLeft rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
handleNewExpressionTreeFromLeft (tokenOp:xs) tree@UnaryNode     {} = let (rest, secondTree) = getExpressionTreeFromLeft xs in handleNewExpressionTreeFromLeft rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
handleNewExpressionTreeFromLeft (op     :xs) tree@BinaryNode    {} = let (rest, secondTree) = getExpressionTreeFromLeft xs in handleNewExpressionTreeFromLeft rest $ placeBinaryOpTokenInTree tree op secondTree
handleNewExpressionTreeFromLeft []                finalTree        = finalTree

placeBinaryOpTokenInTree :: ExpressionTree -> EL.Token -> ExpressionTree -> ExpressionTree
placeBinaryOpTokenInTree base@Leaf           {}             tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@ProtectedNode  {}             tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@UnaryNode      {}             tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(BinaryNode    op left right) tokenOp tree
  | getBinaryOpPrio (tokenToBinaryOp tokenOp) > getBinaryOpPrio op       = BinaryNode op left $ placeBinaryOpTokenInTree right tokenOp tree
  | otherwise                                                            = BinaryNode (tokenToBinaryOp tokenOp) base tree

tokenToBinaryOp :: EL.Token -> BinaryOperator
tokenToBinaryOp EL.Addition       = Addition
tokenToBinaryOp EL.Substraction   = Substraction
tokenToBinaryOp EL.Multiplication = Multiplication
tokenToBinaryOp EL.Division       = Division
tokenToBinaryOp EL.Power          = Power
tokenToBinaryOp _                 = GHCE.throw $ EPE.ExpressionParserException "Cannot convert Token to BinaryOperator"

getBinaryOpPrio :: BinaryOperator -> Int
getBinaryOpPrio Addition       = 1
getBinaryOpPrio Substraction   = 1
getBinaryOpPrio Multiplication = 2
getBinaryOpPrio Division       = 2
getBinaryOpPrio Power          = 3
