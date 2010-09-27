module ExprParser(Expression(..), BinOp(..), getOp, getSym, parseExpr) where

-- | Import des librairies pour parser, ainsi que de la fonction
-- fromJust, justifiée ici car on fait une recherche dans une liste
-- complète.

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Maybe (fromJust)

-- | On définit une structure de donnée Expression pour construire
-- l'AST, qui est soit une opération binaire, soit un litéral.

data Expression = Op BinOp (Expression) (Expression)
                | Lit Integer     deriving (Show, Eq)

-- | Les opérations binaires, ainsi que des tables de correspondance
-- entre l'opérateur et la fonction ou son symbole

data BinOp = Add | Sub | Tim | Div deriving (Show, Eq)

getOp  o = fromJust $ lookup o [ (Add, (+)), (Sub, (-)), (Tim, (*)), (Div, div) ]
getSym o = fromJust $ lookup o [ (Add, "+"), (Sub, "-"), (Tim, "*"), (Div, "/") ]


-- | Le plus difficile :) On parse l'expression pour construire l'AST

parseExpr e = parse expr "Expression parsing" e

expr :: Parser Expression
expr = buildExpressionParser table term <?> "expression"

table :: OperatorTable Char () Expression
table   = [ [op Tim, op Div] , [op Add, op Sub] ] where 
  op o = Infix ((string $ getSym o) >> (return $ Op o)) AssocLeft

trimSpaces p = spaces >> p >>= \r -> spaces >> return r

term  = do{ spaces >> char '(' ; x <- trimSpaces expr ; char ')' >> spaces ; return x }
        <|> do{ ds <- trimSpaces (many1 digit) ; return $ Lit (read ds) }
        <?> "simple expression"
