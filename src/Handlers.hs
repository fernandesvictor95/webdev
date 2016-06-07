{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Handlers where
import Import
import Foundation
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Yesod.Static

import Database.Persist.Postgresql

mkYesodDispatch "Adote" pRoutes


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
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid) 
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

-- validação update de pet
postUpdateAnimalR :: AnimalId -> Handler Html
postUpdateAnimalR aid = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess animal -> (runDB $ Database.Persist.Postgresql.replace aid animal) >>= \eiid -> redirect (GerenciarPetsR)
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
            
-- EXCLUIR USUÁRIO
getExcluirUsuarioR :: UsuarioId -> Handler Html
getExcluirUsuarioR uid = do
        runDB $ get404 uid
        runDB $ delete $ uid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect UsuariosR
        
-- EXCLUIR ANIMAL
getExcluirAnimalR :: AnimalId -> Handler Html
getExcluirAnimalR aid = do
        runDB $ get404 aid
        runDB $ delete $ aid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect GerenciarPetsR
        
-- EXCLUIR RAÇA
getExcluirRacaR :: RacaId -> Handler Html
getExcluirRacaR rid = do
        runDB $ get404 rid
        runDB $ delete $ rid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect RacasR
        
-- EXCLUIR ESPÉCIE
getExcluirEspecieR :: EspecieId -> Handler Html
getExcluirEspecieR eid = do
        runDB $ get404 eid
        runDB $ delete $ eid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect EspeciesR

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
        animais <- runDB $ (rawSql "SELECT ??, ??, ?? FROM animal INNER JOIN raca ON animal.racaid=raca.id INNER JOIN especie ON animal.especieid = especie.id;" [])::Handler [(Entity Animal, Entity Raca, Entity Especie)]
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
               
-- UPDATE DO ANIMAL
getUpdateAnimalR :: AnimalId -> Handler Html
getUpdateAnimalR aid = do
           (widget, enctype) <- generateFormPost formAnimal
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/cadastro.hamlet")               
               
-- PERFIL DO ANIMAL
getAnimalR :: AnimalId -> Handler Html
getAnimalR aid = do
        pets <- runDB $ (rawSql "SELECT ??, ??, ?? FROM animal INNER JOIN raca ON animal.racaid=raca.id INNER JOIN especie ON animal.especieid = especie.id WHERE animal.id = ?;" [toPersistValue aid])::Handler [(Entity Animal, Entity Raca, Entity Especie)]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/perfilpet.hamlet")

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
