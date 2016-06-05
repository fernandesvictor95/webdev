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

mkYesod "Adote" [parseRoutes|
/ HomeR GET
/login LoginR GET POST                  -- página de login
/inicio InicioR GET                     -- página inicial
/admin AdminR GET                       -- página do admin
/erro ErroR GET                         -- página de erro
/usuario UsuarioR GET POST              -- cadastro de usuário
/perfil/#UsuarioId PerfilR GET          -- perfil de usuário
/cadastro CadastroR GET POST            -- cadastro de animal
/animal/#AnimalId AnimalR GET           -- perfil do animal
/pets PetsR GET                         -- lista de todos os animais (usuário)
/gerenciar_pets GerenciarPetsR GET      -- lista de todos os animais (admin)
/cad_especie CadEspecieR GET POST       -- cadastro de espécies
/cad_raca CadRacaR GET POST             -- cadastro de raças
/usuarios UsuariosR GET                 -- lista de usuários
/especies EspeciesR GET                 -- lista de espécies
/racas RacasR GET                       -- lista de raças
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
          areq textField "Nome: " Nothing <*>
          areq textField "Idade: " Nothing <*>
          areq textField "Peso: " Nothing <*>
          areq textField "Tamanho: " Nothing <*>
          areq textField "Obs.: " Nothing <*>
          areq (selectField listarEspecies) "Espécie" Nothing <*>
          areq (selectField listarRacas) "Raça" Nothing

listarEspecies = do
       entidades <- runDB $ selectList [] [Asc EspecieNome] 
       optionsPairs $ fmap (\ent -> (especieNome $ entityVal ent, entityKey ent)) entidades
       
listarRacas = do
       entidades <- runDB $ selectList [] [Asc RacaNome] 
       optionsPairs $ fmap (\ent -> (racaNome $ entityVal ent, entityKey ent)) entidades
          
formRaca :: Form Raca
formRaca = renderDivs $ Raca <$> 
        areq textField "Nome :" Nothing
        
formEspecie :: Form Especie
formEspecie = renderDivs $ Especie <$> 
        areq textField "Nome: " Nothing

----------------------- POSTS

-- página de login
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuarioLogin ==. login, UsuarioSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)
               _ -> redirect ErroR
               
-- validação cadastro de usuário
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect InicioR 
               _ -> redirect ErroR


-- validação cadastro de pet
postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess animal -> (runDB $ insert animal) >>= \aiid -> redirect (AnimalR aiid)
               _ -> redirect ErroR

-- validação cadastro de espécie
postCadEspecieR :: Handler Html
postCadEspecieR = do
           ((result, _), _) <- runFormPost formEspecie
           case result of 
               FormSuccess especie -> (runDB $ insert especie) >>= \eiid -> redirect (CadEspecieR)
               _ -> redirect ErroR
               
               
-- validação cadastro de raça
postCadRacaR :: Handler Html
postCadRacaR = do
           ((result, _), _) <- runFormPost formRaca
           case result of 
               FormSuccess raca -> (runDB $ insert raca) >>= \riid -> redirect (CadRacaR)
               _ -> redirect ErroR


----------------------- GETS


-- CADASTRO DO USUÁRIO
getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/usuario.hamlet")
           


-- PERFIL DO USUÁRIO
getPerfilR :: UsuarioId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          addStylesheet $ StaticR style_css
          toWidget $ $(whamletFile "templates/perfil.hamlet")

-- PÁGINA COM TODOS OS USUÁRIOS
getUsuariosR :: Handler Html
getUsuariosR = do
        listaUsuarios <- runDB $ selectList [] [Asc UsuarioNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listusers.hamlet")

-- PÁGINA COM TODOS OS PETS
getGerenciarPetsR :: Handler Html
getGerenciarPetsR = do
        listaAnimal <- runDB $ selectList [] [Asc AnimalNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listanimais.hamlet")

-- PÁGINA COM TODOS OS PETS
getPetsR :: Handler Html
getPetsR = do
        listaAnimal <- runDB $ selectList [] [Asc AnimalNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/animal.hamlet")
            
-- PÁGINA COM TODAS AS RAÇAS
getRacasR :: Handler Html
getRacasR = do
        listaRacas <- runDB $ selectList [] [Asc RacaNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listracas.hamlet")
            
-- PÁGINA COM TODAS AS ESPÉCIES
getEspeciesR :: Handler Html
getEspeciesR = do
        listaEspecies <- runDB $ selectList [] [Asc EspecieNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listespecies.hamlet")
           
-- CADASTRO DO ANIMAL
getCadastroR ::  Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formAnimal
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/doacoes.hamlet")

-- PERFIL DO ANIMAL
getAnimalR :: AnimalId -> Handler Html
getAnimalR aid = do
      user <- runDB $ get404 aid
      defaultLayout $ do
          addStylesheet $ StaticR style_css
          toWidget $ $(whamletFile "templates/perfil.hamlet")

-- CADASTRO DE ESPÉCIE
getCadEspecieR ::  Handler Html
getCadEspecieR = do
           (widget, enctype) <- generateFormPost formEspecie
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/especies.hamlet")
               
-- CADASTRO DE RAÇA
getCadRacaR ::  Handler Html
getCadRacaR = do
           (widget, enctype) <- generateFormPost formRaca
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/racas.hamlet")

-- PÁGINA INICIAL (ANTES DO LOGIN)           
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/home.hamlet")
               
-- PÁGINA INICIAL (DEPOIS DO LOGIN)
getInicioR :: Handler Html
getInicioR = defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/inicio.hamlet")


-- PÁGINA INICIAL DO ADMIN
getAdminR :: Handler Html
getAdminR = defaultLayout $ do
           addStylesheet $ StaticR style_css
           toWidget $ $(whamletFile "templates/admin.hamlet")

-- PÁGINA DE LOGIN (FORMULÁRIO)
getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ do
           addStylesheet $ StaticR style_css
           toWidget $ $(whamletFile "templates/login.hamlet")

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
--nulpotente

--Get -----------------> consulta (nao cria recurso nao faz nada com servidor)[select]


--Post ----------------> Criação ( cria imagem, registro ou qualquer outra coisa)[ Insert]


  --   IDEMPOTENTE - (Não realiza mais que 1)    
--Put -----------------> Modificação (alteração de resgistro)[Update]


--		IDEMPOTENTE
--Delete --------------> Apagar (Apaga registro)[Delete]


--Options ------------> Informa os metodos https permitidos para o recurso []

{- 

getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]
        
-}