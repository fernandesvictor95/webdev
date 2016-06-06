{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Main where
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Maybe
import Foundation
import Handlers
import Import
import Text.Lucius
import Control.Applicative
import Control.Monad.Logger (runStdoutLoggingT)
import Yesod.Static
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

--data Adote = Adote {getStatic :: Static, connPool :: ConnectionPool }
                 


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