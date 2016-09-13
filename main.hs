{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clientes json
   nome Text
   deriving Show
   
Produto json
   nome Text
   valor Double
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/cadastro UserR GET POST
/cadastroProduto ProdR GET POST
/cadastro/check/#ClientesId CheckR GET
/cadastro/update/#ClientesId UpdateR PUT
/cadastro/delete/#ClientesId DeleteR DELETE
/cadastro/lista ListaR GET
/cadastro/checkNome/#Text PesqR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

getUserR :: Handler Html
getUserR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <form>
        Nome: <input type="text" id="usuario">
    <button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{UserR}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#usuario").val()}),
                 success: function(data) {
                     alert(data.resp);
                     $("#usuario").val("");
                 }
            })
         });
     }
  |]
  
getProdR:: Handler Html
getProdR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <h2>Cadastro de Produtos
    <form>
        Nome: <input type="text" id="nm_produto">
        Valor: <input type="number" id="vl_produto">
    <button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{ProdR}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#nm_produto").val(),"valor":parseFloat($("#vl_produto").val())}),
                 success: function(data) {
                     alert(data.resp);
                     $("#nm_produto").val("");
                     $("#vl_produto").val("");
                 }
            })
         });
     }
  |]

postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

---------------------------------------------------------------------------------------------------------------------

--PARTE DO PRODUTO
----------------------------------------------------------------------------------------
    


postProdR :: Handler ()
postProdR = do
    produtos <- requireJsonBody :: Handler Produto
    runDB $ insert produtos
    sendResponse (object [pack "resp" .= pack "CREATEDPRODUCT"])
    -- Linha 60: Le o json {nome:"Teste"} e converte para
    -- Cliente "Teste". O comando runDB $ insert (Clientes "Teste")
    -- {resp:"CREATED"}


getCheckR :: ClientesId -> Handler ()
getCheckR pid = do
    cli <- runDB $ get404 pid
    sendResponse $ toJSON cli


getListaR :: Handler ()
getListaR = do
    allClientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allClientes])
    

putUpdateR :: ClientesId -> Handler ()
putUpdateR pid = do
    cli <- requireJsonBody :: Handler Clientes
    runDB $ update pid [ClientesNome =. clientesNome cli]
    sendResponse (object [pack "resp" .= pack "UPDATED"])
    

deleteDeleteR :: ClientesId -> Handler ()
deleteDeleteR pid = do
    runDB $ delete pid
    sendResponse (object [pack "resp" .= pack "DELETED"])
    
getPesqR :: Text -> Handler ()
getPesqR ptext = do
    cli <- runDB $ get404 ptext ClientesNome
    sendResponse $ toJSON cli
    
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Ola Mundo
|] 

connStr = "dbname=d5rq97o0klocmf host=ec2-23-21-165-183.compute-1.amazonaws.com user=nrqrmhgbllkdpm password=sCbudbyKiex_yyIQd_sfZkeGo3 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)

{-
curl https://hask2-romefeller.c9users.io/cadastro \
  -v \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"nome":"EU"}'
 -}