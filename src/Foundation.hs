{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Foundation where
import Import
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Maybe
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)
import Yesod.Static
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    
    
data Adote = Adote {getStatic :: Static, connPool :: ConnectionPool }
                 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario json
   nome Text
   cpf Text
   cidade Text
   email Text
   endereco Text
   telefone Text
   login Text
   senha Text
   deriving Show
   
Animal json
   nome Text
   idade Text
   peso Text
   tamanho Text
   observacoes Text
   especieid EspecieId 
   racaid RacaId
   deriving Show
   
Especie json
    nome Text
    deriving Show
   
Raca json
    nome Text
    deriving Show
   
|]

staticFiles "static"

mkYesodData "Adote" pRoutes

instance Yesod Adote where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized PetsR _ = return Authorized
    isAuthorized InicioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized CadEspecieR _ = isAdmin
    isAuthorized CadRacaR _ = isAdmin
    isAuthorized UsuariosR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser um administrador para acessar essa página"
    

instance YesodPersist Adote where
   type YesodPersistBackend Adote = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Adote FormMessage where
    renderMessage _ _ = defaultFormMessage
