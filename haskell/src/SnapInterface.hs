module SnapInterface (runSnapUI) where

import Snap.Http.Server
import Snap.Types
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))

import ExprParser (Expression(..), parseExpr)
import ExprProcessor (eval, toPrefix, toInfix, toPostfix)
import Control.Monad (when)
import Data.List (find)

type TransFix = Expression -> String

runSnapUI :: IO ()
runSnapUI = do
  putStrLn "En écoute sur http://localhost:8000"
  (httpServe "*" 8000 "Alaska Server"
   Nothing Nothing
   router)


router = (method GET showForm) <|> (method POST handleForm)

showForm :: Snap ()
showForm = writeBS formContent

handleForm :: Snap ()
handleForm = do
  me <- getParam "expression"
  mf <- getParam "fixity"
  writeBS $ case (computeRes me mf) of
    Just s -> s
    Nothing -> "Erreur dans le formulaire, essaye encore"

computeRes :: Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString
computeRes me mf = do
  e   <- me
  pe <- maybeExpr e
  mtf <- fmap fixity mf
  tf  <- mtf
  return (B.pack $ concat ["L'expression est : ",tf pe,"\n",
                           "Sa valeur est : ", show $ eval pe])
  
maybeExpr :: B.ByteString -> Maybe Expression
maybeExpr e = case parseExpr (B.unpack e) of 
  Right t -> Just t
  Left _ ->  Nothing

fixity :: B.ByteString -> Maybe TransFix
fixity f =  lookup f [("prefix", toPrefix),
                      ("infix", toInfix),
                      ("postfix", toPostfix)]

formContent :: B.ByteString
formContent = "<HTML>\
\  <HEAD>\
\    <TITLE>Alaska, interface Web</TITLE>\
\  </HEAD>\
\  <BODY>\
\    <FORM action='/' method='post'>\
\      <P>\
\       <LABEL for='expression'>Saisir l'expression en Infix: </LABEL>\
\        <INPUT type='text' name='expression'><BR>\
\       <INPUT type='radio' name='fixity' value='prefix'> Prefix <BR>\
\       <INPUT type='radio' name='fixity' value='infix'>  Infix <BR>\
\       <INPUT type='radio' name='fixity' value='postfix'> Postfix <BR>\
\       <INPUT type='submit' value='Send'> <INPUT type='reset'>\
\      </P>\
\    </FORM>\
\  </BODY>\
\</HTML>"
