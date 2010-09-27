-- | C'est parti pour le convertisseur Infix -> (Prefix | Postfix)
-- interactif. J'ai pris soin d'annoter les fonctions pour que leur
-- type soit explicite.

import ExprParser
import ExprProcessor

-- | Enfin l'interface, avec une fonction main et deux fonctions pour
-- l'aider un peu

main = do
  ex <- getExpr
  putStrLn $ concat ["L'expression vaut ", show $ eval ex]
  fix <- getFix
  putStrLn $ fix ex

getExpr :: IO Expression
getExpr = putStrLn "Entrer expression" >> getLine >>= \l -> 
  case parseExpr l of 
    Right t -> return t 
    Left e -> (putStrLn $ show e) >> getExpr

getFix :: IO (Expression -> String)
getFix = putStrLn "Convertir en prefix (<), infix (.) ou postfix (>) ?" >> 
         getLine >>= \p -> case p of 
           "<" -> return toPrefix
           "." -> return toInfix
           ">" -> return toPostfix
           _   -> getFix
