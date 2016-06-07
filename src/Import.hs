{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Import where

import Yesod
import Yesod.Static

--mkYesod "Adote" 

pRoutes = [parseRoutes|
/ HomeR GET
/login LoginR GET POST                  -- página de login
/inicio InicioR GET                     -- página inicial
/admin AdminR GET                       -- página do admin
/erro ErroR GET                         -- página de erro
/usuario UsuarioR GET POST              -- cadastro de usuário
/perfil/#UsuarioId PerfilR GET          -- perfil de usuário
/excluir_usuario/#UsuarioId ExcluirUsuarioR GET --excluir usuário
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