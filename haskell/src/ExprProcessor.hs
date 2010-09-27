module ExprProcessor where

import ExprParser

-- | On convertit en notation prefix, infix ou postfix
toPrefix, toInfix, toPostfix :: Expression -> String
toPrefix (Op o l r) = concat [getSym o, toPrefix l, toPrefix r]
toPrefix (Lit l) = ' ' : (show l)

toPostfix (Op o l r) = concat [toPostfix l, toPostfix r, getSym o]
toPostfix (Lit l) = ' ' : (show l)

toInfix (Op o l r) = concat ["(",toInfix l, getSym o, toInfix r, ")"]
toInfix (Lit l) = show l

-- | Cette fonction est pratique pour évaluer un arbre (HS pour le moment)
eval :: Expression -> Integer
eval (Op o l r) = op (eval l) (eval r) where
  op = getOp o
eval (Lit l) = l
