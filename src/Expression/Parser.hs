--
-- EvalExpr
-- File description:
-- Expression.Parser
--

module Expression.Parser                ( UnaryOperator(..)
                                        , BinaryOperator(..)
                                        , ExpressionTree(..)
                                        , parse
                                        ) where

import GHC.Exception                    ( throw )

import qualified Argument.Parser             as AP  ( Expression(..) )
import qualified Expression.Lexer            as EL  ( Token(..)
                                                    , OperandType
                                                    , tokenize
                                                    )

import qualified Expression.Parser.Exception as EPE ( ExpressionParserException( ExpressionParserException ) )


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
parseTokens = uncurry newPrioToTree . newNonprioToTree

newPrioToTree :: [EL.Token] -> ExpressionTree -> ExpressionTree
newPrioToTree []           finalTree             = finalTree
newPrioToTree (tokenOp:xs) tree@Leaf          {} = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (tokenOp:xs) tree@ProtectedNode {} = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (tokenOp:xs) tree@UnaryNode     {} = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (op:xs)      tree@BinaryNode    {} = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ placeBinaryOpTokenInTree tree op secondTree

placeBinaryOpTokenInTree :: ExpressionTree -> EL.Token -> ExpressionTree -> ExpressionTree
placeBinaryOpTokenInTree base@(Leaf          _            ) tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(ProtectedNode _            ) tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(UnaryNode     _  _         ) tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(BinaryNode    op left right) tokenOp tree
  | isBinaryOpHigherPrio (tokenToBinaryOp tokenOp) op                 = BinaryNode op left $ placeBinaryOpTokenInTree right tokenOp tree
  | otherwise                                                         = BinaryNode (tokenToBinaryOp tokenOp) base tree

newNonprioToTree :: [EL.Token] -> ([EL.Token], ExpressionTree)
newNonprioToTree        (EL.Operand           op:xs) = (xs, Leaf op)
newNonprioToTree        (EL.Addition            :xs) = wrapTreeAroundUnaryNode (newNonprioToTree xs) Plus
newNonprioToTree        (EL.Substraction        :xs) = wrapTreeAroundUnaryNode (newNonprioToTree xs) Minus
newNonprioToTree tokens@(EL.OpenedParenthesis   :_ ) = let (expr, rest) = getSubtokens tokens in (rest, ProtectedNode $ parseTokens expr)
newNonprioToTree        (EL.ClosedParenthesis   :_ ) = throw $ EPE.ExpressionParserException "Mismatched parentheses"
newNonprioToTree        _                            = throw $ EPE.ExpressionParserException "An operator is missing its operand(s)"

wrapTreeAroundUnaryNode :: ([EL.Token], ExpressionTree) -> UnaryOperator -> ([EL.Token], ExpressionTree)
wrapTreeAroundUnaryNode (rest, tree) operator = (rest, UnaryNode operator tree)

getSubtokens :: [EL.Token] -> ([EL.Token], [EL.Token])
getSubtokens (EL.OpenedParenthesis:xs) = getSubtokensMatch ([], xs) 0
getSubtokens _                         = throw $ EPE.ExpressionParserException "Trying to parse subexpression without opened parenthesis at its start"

getSubtokensMatch :: ([EL.Token], [EL.Token]) -> Int -> ([EL.Token], [EL.Token])
getSubtokensMatch (subtokens, EL.ClosedParenthesis:xs) 0       = (reverse subtokens, xs)
getSubtokensMatch (subtokens, EL.ClosedParenthesis:xs) nbMatch = getSubtokensMatch (EL.OpenedParenthesis:subtokens, xs) $ nbMatch - 1
getSubtokensMatch (subtokens, EL.OpenedParenthesis:xs) nbMatch = getSubtokensMatch (EL.OpenedParenthesis:subtokens, xs) $ nbMatch + 1
getSubtokensMatch (subtokens, x:xs)                    nbMatch = getSubtokensMatch (x:subtokens, xs) nbMatch
getSubtokensMatch (_, [])                              _       = throw $ EPE.ExpressionParserException "Mismatched parentheses"

tokenToBinaryOp :: EL.Token -> BinaryOperator
tokenToBinaryOp EL.Addition       = Addition
tokenToBinaryOp EL.Substraction   = Substraction
tokenToBinaryOp EL.Multiplication = Multiplication
tokenToBinaryOp EL.Division       = Division
tokenToBinaryOp EL.Power          = Power
tokenToBinaryOp _                 = throw $ EPE.ExpressionParserException "Cannot convert Token to BinaryOperator"

isBinaryOpHigherPrio :: BinaryOperator -> BinaryOperator -> Bool
isBinaryOpHigherPrio left right = getBinaryOpPrio left > getBinaryOpPrio right

getBinaryOpPrio :: BinaryOperator -> Int
getBinaryOpPrio Addition       = 1
getBinaryOpPrio Substraction   = 1
getBinaryOpPrio Multiplication = 2
getBinaryOpPrio Division       = 2
getBinaryOpPrio Power          = 3
