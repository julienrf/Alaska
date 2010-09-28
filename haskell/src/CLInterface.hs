module CLInterface(runCLI) where

import ExprParser (Expression(..), parseExpr)
import ExprProcessor (eval, toPrefix, toInfix, toPostfix)
import Control.Monad (when)

type TransFix = Expression -> String

data UserState = UserState { expression :: Expression,
                             fixity     :: TransFix, 
                             playAgain  :: Bool}

runCLI = mainMenu baseState where
  baseState = UserState { expression = (Lit 0), fixity = toInfix, playAgain = True }

mainMenu :: UserState -> IO ()
mainMenu ps = do
  s <- updateState ps
  when (playAgain s) $ do
    putStrLn $ concat [" - L'expression est  ", (fixity s) (expression s)]
    putStrLn $ concat [" - Sa valeur est     ", show $ eval (expression s)]
    mainMenu s


updateState :: UserState -> IO UserState
updateState s = do
  putStrLn $ unlines ["","Choisir une action",
                      "[f] Régler le format de sortie",
                      "[e] Saisir une expression",
                      "[q] Quitter"]
  getLine >>= \c ->  case c of
    "f" -> getFix >>= \f -> return $ s { fixity = f }
    "e" -> getExpr >>= \e ->  return $ s { expression = e }
    "q" -> return $ s { playAgain = False }
    _   -> updateState s
  

getExpr :: IO Expression
getExpr = putStrLn "Entrer l'expression : " >> getLine >>= \l -> 
  case parseExpr l of 
    Right t -> return t 
    Left e -> (putStrLn $ show e) >> getExpr

getFix :: IO TransFix
getFix = (putStrLn $ unlines ["","Afficher en notation",
                              "[<] prefix",
                              "[.] infix",
                              "[>] postfix"] ) >> 
         getLine >>= \p -> case p of 
           "<" -> return toPrefix
           "." -> return toInfix
           ">" -> return toPostfix
           _   -> getFix
