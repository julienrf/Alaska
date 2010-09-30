module SnapInterface (runSnapUI) where

import Snap.Http.Server
import Snap.Types
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))

import ExprParser (Expression(..), parseExpr)
import ExprProcessor (eval, toPrefix, toInfix, toPostfix)

type TransFix = Expression -> String

runSnapUI :: Int -> IO ()
runSnapUI port = do
  putStrLn "En écoute sur http://localhost:8000"
  (httpServe "*" port "Alaska Server"
   Nothing Nothing
   router)


router, showForm, handleForm :: Snap ()
router = (method GET showForm) <|> (method POST handleForm)

showForm = writeBS formContent

-- Attention, on est dans la snap monad
handleForm = do
  me <- getParam "expression"
  mf <- getParam "fixity"
  writeBS $ case (tryComputeRes me mf) of
    Right s -> B.pack s
    Left  e -> B.pack $ concat ["Erreur : ", show e]

tryComputeRes :: Maybe B.ByteString -> Maybe B.ByteString 
                 -> Either String String
tryComputeRes me mf = 
  case me of
    Nothing -> Left "Expression is missing"
    Just e  -> case fmap fixity mf of
      Nothing -> Left "Fixity is missing"
      Just mf' -> case mf' of
        Nothing -> Left "Wrong fixity"
        Just f  -> case parseExpr (B.unpack e) of
          Left err -> Left $ concat ["Parse Error : ", show err]
          Right pe -> Right $ concat ["L'expression est : ",f pe,"\n",
                                      "Sa valeur est : ", show $ eval pe]
        
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
