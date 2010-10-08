module SnapInterface (runSnapUI) where

import Snap.Http.Server
import Snap.Types
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))

import ExprParser (Expression(..), parseExpr)
import ExprProcessor (eval, toPrefix, toInfix, toPostfix)

type TransFix = Expression -> String

-- | Transforme un maybe en either, utile pour insérer un message
-- d'erreur
maybeToEither :: e -> Maybe v -> Either e v
maybeToEither = flip maybe Right . Left


-- | Le getParam de base, simple Maybe, ne permet pas de mettre un
-- message d'erreur en cas de paramètre manquant.
getParamEither :: String -> Snap (Either String B.ByteString)
getParamEither p = getParam (B.pack p) >>= \ mp -> 
  return $ maybeToEither ("Missing parameter " ++ p) mp 

runSnapUI :: Int -> IO ()
runSnapUI port = do
  putStrLn $ "En écoute sur http://localhost:" ++ (show port)
  (httpServe "*" port "Alaska Server"
   Nothing Nothing
   router)

router, showForm, handleForm :: Snap ()
router = (method GET showForm) <|> (method POST handleForm)

showForm = writeBS formContent

-- Attention, on est dans la snap monad
handleForm = do
  ee <- getParamEither "expression"
  ef <- getParamEither "fixity"
  writeBS $ case (tryComputeRes ee ef) of
    Right s -> B.pack s
    Left  e -> B.pack $ concat ["Erreur : ", show e]

tryComputeRes :: Either String B.ByteString -> Either String B.ByteString 
                 -> Either String String
tryComputeRes ee esf = do
  e <- ee
  sf <- esf
  f <- maybeToEither "Fixity is invalid" $ fixity sf
  case parseExpr (B.unpack e) of
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

