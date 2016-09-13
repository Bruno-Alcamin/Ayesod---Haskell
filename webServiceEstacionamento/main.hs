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

Cliente json
   nome Text
   flcliente Text
   deriving Show

Telefone json
   ordem Text
   ClienteId required Foreign Key
   deriving Show
   
ClienteFisico json
   rg Text
   sexo Text
   cpf Text
   ClienteId required Foreign Key
   deriving Show
   
Endereco json
    logradouro Text
    cidade Text
    estado Text
    bairro Text
    cep Text
    ClienteId required Foreign Key
    deriving Show

Contrato json
    valor Double
    contratoinc Day
    contratofim Day
    quantidadeparcela Int 
    quantidadevagas Int
    ClienteId required Foreign Key
    deriving Show

Parcela json
    dataparcela Day
    valor Double
    valorpago Double
    datapagamento
    ContratoId required Foreign Key
    deriving Show
    
Evento json
    descricao Text
    percentualDesconto Int
    ContratoId required Foreign Key
    deriving Show
    
ClienteJuridico json
    cnpj Text
    razaosocial Text
    ClienteId required Foreign Key
    deriving Show

TipoVeiculo json
    nome Text
    deriving Show

Veiculo json
    placa Text
    descricao Text
    marca Text
    ano Text
    cor Text
    TipoVeiculoId required Foreign Key
    ClienteId required Foreign Key
    deriving Show
    
VagaValor json
    valordiurno Double
    valornoturno Double
    deriving Show
    
Vaga json
    diurno Text
    noturno Text
    VagaValorId required Foreign Key
    deriving Show
    
ContratoVaga json
    periodo Text
    ContratoId required Foreign Key
    VagaId required Foreign Key
    deriving Show
    
Conveniado json
    nome Text
    percentualDesconto Double
    EventoId optional Foreign Key
    deriving Show
    
Avulso json
    placa Text
    entrada Day
    saida Day
    valor Double
    VagaId required Foreign Key
    ConveniadoId optional Foreign Key
    deriving Show
    
Funcionario json
    nome Text
    senha Text --talvaz
    deriving Show

HistoricoPreco json
    dataalteracao Day
    valordiurnoantigo Double
    valornoturnoantigo Double
    valordiurnonovo Double
    valordiurnonovo Double
    VagaId required Foreign Key
    FuncionarioId required Foreign Key
    deriving Show
|]



mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/cadastrocliente ClienteR GET POST
/cadastroveiculo VeiculoR GET POST
/cadastrotipoveiculo TipoVeiculoR GET POST
/cadastrocontrato ContratoR GET POST
/cadastrovaga VagaR GET POST
/cadastrovagavalor VagaValorR GET POST
/cadastroevento EventoR GET POST
/cadastroconveniado ConveniadoR GET POST
/avulso AvulsoR GET POST -- pode ser entrada de veiculo também
/cadastrofuncionario FuncionarioR GET POST

--/cadastro/check/#ClientesId CheckR GET
-- para cada cadastro vai ter uma rota de alterar e deletar
/cadastro/update/#ClientesId UpdateR PUT
/cadastro/delete/#ClientesId DeleteR DELETE
/cadastro/lista ListaR GET

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
    
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Estacionamento BGM
|] 

connStr = "dbname=Estacionamento host=127.0.0.1 user=bruno_alcamin password= port=80"

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
  
  
  Iniciando mysql
mysql-ctl start

phpmyadmin-ctl install

 Username: bruno_alcamin
 -}
 
 