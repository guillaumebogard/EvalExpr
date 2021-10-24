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
parseTokenizedExpression = uncurry newPrioToTree . newNonprioToTree

newPrioToTree :: [Token] -> ExpressionTree -> ExpressionTree
newPrioToTree []           finalTree             = finalTree
newPrioToTree (tokenOp:xs) tree@Leaf {}          = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (tokenOp:xs) tree@ProtectedNode {} = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (tokenOp:xs) tree@UnaryNode {}     = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ BinaryNode (tokenToBinaryOp tokenOp) tree secondTree
newPrioToTree (op:xs)      tree@BinaryNode {}    = let (rest, secondTree) = newNonprioToTree xs in newPrioToTree rest $ placeBinaryOpTokenInTree tree op secondTree

placeBinaryOpTokenInTree :: ExpressionTree -> Token -> ExpressionTree -> ExpressionTree
placeBinaryOpTokenInTree base@(Leaf _)                   tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(ProtectedNode _)          tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(UnaryNode _ _)            tokenOp tree = BinaryNode (tokenToBinaryOp tokenOp) base tree
placeBinaryOpTokenInTree base@(BinaryNode op left right) tokenOp tree
  | isBinaryOpHigherPrio (tokenToBinaryOp tokenOp) op                 = BinaryNode op left $ placeBinaryOpTokenInTree right tokenOp tree
  | otherwise                                                         = BinaryNode (tokenToBinaryOp tokenOp) base tree

newNonprioToTree :: [Token] -> ([Token], ExpressionTree)
newNonprioToTree (EL.OPERAND operand:xs)          = (xs, Leaf operand)
newNonprioToTree (EL.ADDITION:xs)                 = wrapTreeAroundUnaryNode (newNonprioToTree xs) ExpressionParser.PLUS
newNonprioToTree (EL.SUBSTRACTION:xs)             = wrapTreeAroundUnaryNode (newNonprioToTree xs) ExpressionParser.MINUS
newNonprioToTree tokens@(EL.OPENED_PARENTHESIS:_) = let (expr, rest) = getSubtokens tokens in (rest, ProtectedNode $ parseTokenizedExpression expr)
newNonprioToTree (EL.CLOSED_PARENTHESIS:_)        = throw $ InvalidExpressionError "Mismatched parentheses"
newNonprioToTree _                                = throw $ InvalidExpressionError "An operator is missing (an) operand(s)"

wrapTreeAroundUnaryNode :: ([Token], ExpressionTree) -> UnaryOperator -> ([Token], ExpressionTree)
wrapTreeAroundUnaryNode (rest, tree) operator = (rest, UnaryNode operator tree)

getSubtokens :: [Token] -> ([Token], [Token])
getSubtokens (EL.OPENED_PARENTHESIS:xs) = getSubtokensMatch ([], xs) 0
getSubtokens _                          = throw $ InvalidExpressionError "Trying to parse subexpression without opened parenthesis at its start"

getSubtokensMatch :: ([Token], [Token]) -> Int -> ([Token], [Token])
getSubtokensMatch (subtokens, EL.CLOSED_PARENTHESIS:xs) 0        = (reverse subtokens, xs)
getSubtokensMatch (subtokens, EL.CLOSED_PARENTHESIS:xs) nb_match = getSubtokensMatch (EL.CLOSED_PARENTHESIS:subtokens, xs) $ nb_match - 1
getSubtokensMatch (subtokens, EL.OPENED_PARENTHESIS:xs) nb_match = getSubtokensMatch (EL.OPENED_PARENTHESIS:subtokens, xs) $ nb_match + 1
getSubtokensMatch (subtokens, x:xs)                     nb_match = getSubtokensMatch (x:subtokens, xs) nb_match
getSubtokensMatch (_, [])                               _        = throw $ InvalidExpressionError "Mismatched parentheses"

tokenToBinaryOp :: Token -> BinaryOperator
tokenToBinaryOp EL.ADDITION       = ExpressionParser.ADDITION
tokenToBinaryOp EL.SUBSTRACTION   = ExpressionParser.SUBSTRACTION
tokenToBinaryOp EL.MULTIPLICATION = ExpressionParser.MULTIPLICATION
tokenToBinaryOp EL.DIVISION       = ExpressionParser.DIVISION
tokenToBinaryOp EL.POWER          = ExpressionParser.POWER
tokenToBinaryOp _                 = throw $ InvalidExpressionError "Cannot convert Token to BinaryOperator"

isBinaryOpHigherPrio :: BinaryOperator -> BinaryOperator -> Bool
isBinaryOpHigherPrio left right = getBinaryOpPrio left > getBinaryOpPrio right

getBinaryOpPrio :: BinaryOperator -> Int
getBinaryOpPrio ExpressionParser.ADDITION       = 1
getBinaryOpPrio ExpressionParser.SUBSTRACTION   = 1
getBinaryOpPrio ExpressionParser.MULTIPLICATION = 2
getBinaryOpPrio ExpressionParser.DIVISION       = 2
getBinaryOpPrio ExpressionParser.POWER          = 3
