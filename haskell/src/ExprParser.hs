module ExprParser(Expression(..), OpName(..), getOp, getSym, parseExpr) where

-- | Import des librairies pour parser, ainsi que de la fonction
-- fromJust, justifiée ici car on fait une recherche dans une liste
-- complète.

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Maybe (fromJust)

-- | On définit une structure de donnée Expression pour construire
-- l'AST, qui est soit une opération binaire, soit un litéral.

data Expression = Op OpName (Expression) (Expression)
                | Lit Integer     deriving (Show, Eq)

-- | Les opérations binaires, ainsi que des tables de correspondance
-- entre l'opérateur et la fonction ou son symbole

data OpName = Add | Sub | Tim | Div deriving (Show, Eq)
type BinOp = Integer -> Integer -> Integer

getOp :: OpName -> BinOp
getOp  o = fromJust $ lookup o [ (Add, (+)), (Sub, (-)), (Tim, (*)), (Div, div) ]
getSym :: OpName -> String
getSym o = fromJust $ lookup o [ (Add, "+"), (Sub, "-"), (Tim, "*"), (Div, "/") ]


-- | Le plus difficile :) On parse l'expression pour construire l'AST

parseExpr :: String -> Either ParseError Expression
parseExpr e = parse expr "Expression parsing" e

expr :: Parser Expression
expr = buildExpressionParser table term <?> "expression"

table :: OperatorTable Char () Expression
table   = [ [op Tim, op Div] , [op Add, op Sub] ] where 
  op o = Infix ((string $ getSym o) >> (return $ Op o)) AssocLeft

trimSpaces :: Parser a -> Parser a
trimSpaces p = spaces >> p >>= \r -> spaces >> return r

term :: Parser Expression
term  = do{ _ <- spaces >> char '(' ; x <- trimSpaces expr ; char ')' >> spaces ; return x }
        <|> do{ ds <- trimSpaces (many1 digit) ; return $ Lit (read ds) }
        <?> "simple expression"
