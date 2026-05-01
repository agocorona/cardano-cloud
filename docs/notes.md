TO USE RUNGHC/GHCI when libraries are not found

repeat:
>cabal install --lib  --package-env . <library>

for each library that does not find

https://discourse.haskell.org/t/could-not-find-module-in-ghci/4385/9

packages to install:

sudo apt-get install zlib1g-dev
cabal install  --lib  --package-env . random directory bytestring containers mtl stm bytestring aeson case-insensitive network websockets network-uri old-time base64-bytestring TCache data-default  network-uri network-bsd     text mime-types process vector TCache hashable signal deepseq data-default HTTP time hashtables parsec base16-bytestring cardano-api cardano-binary cardano-ledger-alonzo cardano-ledger-core cardano-ledger-conway cardano-ledger-binary cardano-ledger-api cardano-binary ouroboros-network-protocols

for cardano-cloud:
  cardano-api-10.19.1.0 base16-bytestring-1.0.2.0 cardano-ledger-binary-1.7.0.0  cardano-ledger-alonzo
F
because runghc permits faster iteration:

clear && runghc -w  -threaded -rtsopts -itransient-stack/transient/src -itransient-stack/transient-universe/src -icardano-cloud/src -iTCache cardano-cloud/tests/test.hs --cardanoparams    /opt/cardano/ipc/node.socket  preview ./cardano-cloud/tests/payment.skey

params:
--start localhost:8080 
--cardanoparams   /ipc/node.socket  preview ./cardano-cloud/tests/payment.skey

test publish 
test recovery before sign

streaming in js console

construct a general transaction
integrate chooseutxos

endpoints: RState

con SetIndexData

componentes
------------
getutxos   Payment.hs usando getUTxOsAtIO
selectUtxos  Payment.hs usando UtxoSelector.hs   payment -> balance, quitar utxoselector y que sea independiente
balance  Payment.hs
sign   signSend en Api.hs  
agregate sign   multisignedfromBrowsers    (probado en pay)     multisignedfromBrowsers -> aggregateSign
submit   -> crear submit con backtracking


Payments.hs - su nucleo es balanceMulti, aislarlo de  getUtxos y utxoSelector 


Sync.hs  consultar desde getUTxOsAtIO

0) casos de backtracking
1)  aislar el actual pay en un legacy pay.hs
2) extraer signSend



hacer 
  initWebSession
  initWallet
  initWService


como tratar rollback de una transaccion ya emitida
el nodo se encarga de reenviar la transaccion si es rechazada por saturacion 
   pero si hay un timeout, los utxos hay que liberarlos como disponibles en el cache 
solo los utxos gastados en chains temporales separadas o timeouts hay que tratarlas
en submit
rollbackhandling= do
  time <-  un numero de slots
  timeout time $ do
    chain <- getMailbox
^
ahora
se envia la transaccion
se ponen los utxos como gastados slot
el nodo reintenta la transaccion pero
 si la transaccion tiene timeout se quedan como gastados
 y deberian estar disponibles

que hay que hacer
se envia la transaccion
se ponen los utxos con "en mempool"
el nodo reintenta, pero si se descarta por timeout, la pondra de nuevo en el mempool
  entonces en cuanto se detecta en el mempool esa transaccion, se ponen sus utxos como disponibles
si no, despuesd e un timeout se ponen esos utxos como gastados

--- cache--
tener un programa que sincronice y otro que acceda, para que pueda haber varios programas que accedan al mismo cache. si tienen el mismo codigo:
  runAt syncNode ...

si no, a través de minput

hay otra posibilidad para programas que son parcialmente similares, con zonas similares?:

crear un named endpoint: endpoint $ Just "syncServices"

syncServices= do
   endpoint "syncServices"
   blockSpitter <|> mempoolSpitter <|> ...

en nodo cliente

main ...   <|>syncServices <|>...

remoteBlockSpitter= runAtNamed "syncServices" syncNode blockSpitter
remoteMempoolSpitter= runAtNamed "syncServices" syncNode mempoolSpitter

runatNamed no puede compartir el stack del llamante porque no comparten codigo

variableExterna
estas variables no son accesibles al nodo remoto
result <- runAtNamed node closure funcionLlamada
el tipo de result sirve para general la llamada rest equivalente haciendo minput
el tipo de funcionLlamada genera el parametro payload de minput

funcionLlamada param  = do
   param es serializado en un JSON
   p <- local 4 return param
   mismo codigo en llamante y llamado dentro de runAtName
   estas variables genradas si son compartidas
   no se puede mencionar una variable anterio a runAtNamed o produciria un error de compilacion en el codigo llamado
   

el log se encuentra con una orden endpoint nombre
 ese endpoint como sabe si es un jump o no?
   si el log tuvera info de su anterior endpoint,
     consultaria su anterior y podria saber si es un jump
     endpoint name/closure - stored endpoint
       si es stored endpoint podrá reconstruirse si no existe
       pero tiene que reconstruir el original cont-o?
         no debe reconstruir el original y renombrarlo con su session id

  en en endpoint named poner siempre un jump
    o no poner jump sino directamente itentar refrescarlo
    no es posible ya que no conoce su session id
  cont debe guardar la sesion en la URL
    las published al menos.
  ya conoce su sesion. detectar la sesion

  hay que restaurar las closures porque no se puede asegurar que lo exportado por otro usuario está disponible no solo el admin
     por tanto, tampoco los publish estan disponibles. hay que salvarlos pero solo clos-sesion o DBR

copiar el registro de la sesion del link, grabar uno nuevo con la nueva sesion y enviar a esa closure