module CLInterface(runCLI) where

import ExprParser (Expression(..), parseExpr)
import ExprProcessor (eval, toPrefix, toInfix, toPostfix)
import Control.Monad (when)
import Data.List (find)

type TransFix = Expression -> String

data UserState = UserState { expression :: Expression,
                             fixity     :: TransFix, 
                             playAgain  :: Bool}

runCLI :: IO ()
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
updateState s = ( createMenu
                  "Choisir une action"
                  [("f", "Régler le format de sortie", 
                    getFix >>= \f -> return $ s { fixity = f }),
                   ("e", "Saisir une expression",
                    getExpr >>= \e ->  return $ s { expression = e }),
                   ("q", "Quitter", return $ s { playAgain = False })] )
                 

getExpr :: IO Expression
getExpr = putStrLn "Entrer l'expression : " >> getLine >>= \l -> 
  case parseExpr l of 
    Right t -> return t 
    Left e -> (putStrLn $ show e) >> getExpr

getFix :: IO TransFix
getFix = ( createMenu 
           "Choisir la notation pour l'affichage"
           [("<", "Prefix", return toPrefix),
            (".", "Infix",  return toInfix),
            (">", "Postfix", return toPostfix)] )



createMenu :: String -> [(String, String, IO a)] -> IO a
createMenu title options = do 
  putStrLn ""
  putStrLn $ concat ["--- ", title]
  putStrLn $ unlines $ map (\(k,l,_) -> concat ["[",k,"] ",l]) options
  getLine >>= \c -> case (find (\(k,_,_) -> k == c) options) of
    Just (_,_,a) -> a
    Nothing      -> putStrLn "essaye encore ..." >> createMenu title options
