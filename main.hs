{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
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
   especie Text
   nome Text
   raca Text
   idade Text
   peso Text
   tamanho Text
   observacoes Text
   deriving Show
   
Especie json
    id_especie Int
    nome Text
   
Raca json
    id_raca Int
    nome Text
   
|]

staticFiles "static"

mkYesod "Adote" [parseRoutes|
/ HomeR GET
/login LoginR GET POST                  -- página de login
/erro ErroR GET                         -- página de erro
/usuario UsuarioR GET POST              -- cadastro de usuário
/perfil/#UsuarioId PerfilR GET          -- perfil de usuário
/cadastro CadastroR GET POST            -- cadastro de animal
/animal/#AnimalId AnimalR GET           -- perfil do animal
/pets PetsR GET                         -- menu com todos os pets
/logout LogoutR GET
/static StaticR Static getStatic

|]
 
instance Yesod Adote where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized PetsR _ = return Authorized
    -- isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
{- isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui" -}

instance YesodPersist Adote where
   type YesodPersistBackend Adote = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Adote FormMessage where
    renderMessage _ _ = defaultFormMessage



-- FORMULÁRIOS

-- cadastro de usuário
formUser :: Form Usuario
formUser = renderDivs $ Usuario <$>
          areq textField "Nome: " Nothing <*>
          areq textField "CPF: " Nothing <*>
          areq textField "Cidade: " Nothing <*>
          areq textField "Email: " Nothing <*>
          areq textField "Endereço: " Nothing <*>
          areq textField "Telefone: " Nothing <*>
          areq textField "Login: " Nothing <*>
          areq passwordField "Senha: " Nothing


-- form de login
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
          areq textField "Login: " Nothing <*>
          areq passwordField "Senha: " Nothing
           
-- cadastro de animal           
formAnimal :: Form Animal
formAnimal = renderDivs $ Animal <$>
          areq textField "Espécie: " Nothing <*> 
          areq textField "Nome: " Nothing <*>
          areq textField "Raça: " Nothing <*>
          areq textField "Idade: " Nothing <*>
          areq textField "Peso: " Nothing <*>
          areq textField "Tamanho: " Nothing <*>
          areq textField "Observações: " Nothing        


----------------------- POSTS

-- página de login
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
              -- FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuarioLogin ==. login, UsuarioSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

-- página de cadastro de usuário
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR


-- página de cadastro de pet
postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess animal -> (runDB $ insert animal) >>= \aiid -> redirect (AnimalR aiid)
               _ -> redirect ErroR




----------------------- GETS


-- CADASTRO DO USUÁRIO
getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]
           


-- PERFIL DO USUÁRIO
getPerfilR :: UsuarioId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")


-- PÁGINA COM TODOS OS PETS
getPetsR :: Handler Html
getPetsR = 
           defaultLayout $ do
           toWidget $ $(luciusFile "templates/home.lucius")
           $(whamletFile "templates/animal.hamlet")
           
-- CADASTRO DO ANIMAL
getCadastroR ::  Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviarr">
           |]

getAnimalR :: AnimalId -> Handler Html
getAnimalR aid = do
      user <- runDB $ get404 aid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")


-- PÁGINA INICIAL           
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           toWidget $ $(luciusFile "templates/home.lucius")
           $(whamletFile "templates/home.hamlet")

-- PÁGINA INICIAL DO ADMIN
getAdminR :: Handler Html
getAdminR = defaultLayout $ do
           addStylesheet $ StaticR teste_css
           [whamlet|
               <label> Bem-vindo ao sistema!
               <ul>
                  <li> <a href=@{LoginR}> Cadastro de peca
                  <img src=@{StaticR cachorro_jpg}>
|]

-- PÁGINA DE LOGIN (FORMULÁRIO)
getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ do
           toWidget $ $(luciusFile "templates/login.lucius")
           [whamlet|
           <div id="form_login">
                
                <div id="form_area">
                    <form method=post enctype=#{enctype} action=@{LoginR}>
                       
                            ^{widget}
                        <input type="submit" value="Entrar">
           |]
           
            


-- PÁGINA DE ERRO
getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]


-- MENSAGEM AO FAZER LOGOUT
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> Até a próxima!
     |]

-- CONEXÃO COM O BANCO
connStr = "dbname=de3isbdvftdc7i host=ec2-107-20-174-127.compute-1.amazonaws.com user=yiemztstzaoevg password=0QtySsqXrYze_VNMixKLTTI8Lk port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Adote t pool)



-- stack build (salvar)
-- stack exec -- NomeDaPasta


--REST
	--    nulpotente
--Get -----------------> consulta (nao cria recurso nao faz nada com servidor)[select]


--Post ----------------> Criação ( cria imagem, registro ou qualquer outra coisa)[ Insert]


  --   IDEMPOTENTE - (Não realiza mais que 1)    
--Put -----------------> Modificação (alteração de resgistro)[Update]


--		IDEMPOTENTE
--Delete --------------> Apagar (Apaga registro)[Delete]


--Options ------------> Informa os metodos https permitidos para o recurso []