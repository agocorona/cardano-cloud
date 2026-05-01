-- Store and forward primitives as well a backtracking preserving primitives that allows for
-- transactions in durable and distributed computations with failure recovery.
-- Handlers of backtraking transactions are honored even in presence of failures and shutdowns
-- WORK IN PROGRESS
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module CardanoC.SF (runCC, job, collect, allPublishedEndpoints, trackTxService) where
import Cardano.Api
import CardanoC.Defs
import CardanoC.Sync
import Transient.Base
import Transient.Move
import Transient.Move.Job
import Transient.Console
import Transient.Move.Utils
import Transient.Move.Web
import Transient.Internals(lazy)
import Control.Monad
import Control.Exception hiding(onException)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Applicative
import System.Random
import System.IO
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BSS
import qualified Data.ByteString.Char8 as BS
import Data.Coerce
import Data.TCache
import Data.TCache.IndexQuery
import Control.Concurrent.STM (readTVar)

import Data.IORef


-- | Run the cardano-cloud computation.
-- Connect to the local node
-- restart the active jobs
-- init the REST endpoint for starting the user interaction 
--
-- A typical invocation of the program:
--
-- > program --start localhost:8080 --cardanoparams   /ipc/node.socket  preview ./cardano-cloud/tests/payment.skey
--
-- invoke http://node:port to enter the web app
runCC :: Cloud () -> IO ()
runCC  cl=  void $ keep $ initNode $ inputNodes <|>  do
  runJobs
  ttr "HERE"
  connectCardanoNode
  ttr "AFTER COONECT"
  userInit  <|> return()
  ttr "AFTER USERINIT"
  cl
  where
  connectCardanoNode= onAll $ do
    liftIO $ do
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8
      hSetEncoding stdin  utf8
      hSetBuffering stdout NoBuffering
    (socketPath, networkId, skeyPath) <- cardanoParams
    ttr (socketPath, networkId, skeyPath)
    liftIO $ putStrLn "Initalizing Cardano Cloud environment"
    -- 1. Initialize the environment only once.
    localExceptions $ do
      onException $ \(SomeException e) ->do
        tttr $ "Cardano Node does not run yet. Is the node synchronised? " <> show e
        exitLeft e
        empty
      env <- liftIO $ initAppEnv socketPath networkId skeyPath
      setState env
    fork syncProcess


  initAppEnv
    :: FilePath
    -> NetworkId
    -> FilePath
    -> IO AppEnv
  initAppEnv socketPath networkId skeyPath = do


    -- Local connection
    let conn = LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
                , localNodeNetworkId       = networkId
                , localNodeSocketPath      = File socketPath
                }

    eResult <- runExceptT $ queryNodeLocalState conn VolatileTip
                          (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters))

    pparams <- case eResult of
                Left acquiringErr -> error $ "Failed to acquire connection to the node: " ++ show acquiringErr
                Right (Left eraMismatch) -> error $ "Era mismatch in PParams query: " ++ show eraMismatch
                Right (Right pp) -> return pp  -- pp :: PParams ConwayEra (from ledger, matches your envPParams)
    let currentEra = AnyCardanoEra ConwayEra

    putStrLn "AppEnv initialized correctly!"
    putStrLn $ "Network: " ++ show networkId
    -- putStrLn $ "Own address: " ++ show(serialiseAddress ownAddr)
    putStrLn $ "Socket: " ++ socketPath

    return AppEnv
      { envConn       = conn
      -- , envSigningKey = skey
      -- , envOwnAddress = ownAddr
      , envPParams    = pparams
      , envNetworkId  = networkId
      , envEra        = currentEra
      }



  cardanoParams= do
    option "cardanoparams"  "Enter the Cardano params" :: TransIO String
    socketPath <- do
      input (const True) "Enter the socket path > " :: TransIO String

    -- liftIO $ putStrLn  "Choose the network:"
    networkId <- do
      (n :: String) <- input (const True) "Network id (mainnet,preprod or preview)? >"
      case map Data.Char.toLower n of
        "mainnet" -> return $ Mainnet
        "preprod" -> return $ Testnet (NetworkMagic 1)
        "preview" -> return $ Testnet (NetworkMagic 2)
        s -> error "please enter a valid network id (mainnet, preprod or preview)"

    skeyPath <- input (const True) "Enter the path of your key file > "
    return (socketPath :: String, networkId, skeyPath :: String)


  -- | the account address is initiated and a endpoint to receive all the endpoint available
  userInit :: Cloud ()
  userInit = do
    ttr "USERINIT"
    minput "init" ("init" :: String):: Cloud()
    ttr "AFTER INIT"
    session <- local $ do
            mc <-getCookie "state"
            ttr ("COOKIEE",mc)
            if isJust mc
              then return $ fromJust mc
              else do
                s <- genSessionId
                setCookie "state" $ BSS.fromStrict s <> "; Path=/"
                return s
    onAll $ setSession session
    ttr "AFTER SESSION"
    POSTData useraddr' <- minput "addrs" ("Please connect your wallet" :: String)
    useraddr <- if null $ listPayAddresses useraddr'
      then
        if isNothing (changeAddress useraddr')
          then do
              moutput ("No payment addresses provided, please retry" :: String)
              empty
          else
            return useraddr'{listPayAddresses= [fromJust $ changeAddress useraddr']}
      else return useraddr'

    let userEnv= UserEnv{userAddress= useraddr}
    onAllNodes $ setState userEnv




    local $ do
      getInitialWalletUtxos useraddr
      bal <- liftIO $ calculateBalance $ head $ listStakeAddresses $ userAddress userEnv
      output ("BALANCE: " <> show (bal `div` 1000000) <>"."<> show (bal `mod` 1000000) <> " ADA")

    local $ do
      let stakeAddresses = map normalizeStakeCred $ listStakeAddresses $   userAddress userEnv
      liftIO $ print ("adding stake address",stakeAddresses)
      liftIO $ atomically $ do
        wl <- readDBRef watchList `Data.TCache.onNothing` return (WatchList  Set.empty  Map.empty)
        writeDBRef watchList  wl{ credentials= Set.fromList stakeAddresses `Set.union` credentials wl, vPaymentKeys=vPaymentKeys wl}

      -- wl <- liftIO $ atomically $ readDBRef watchList
      -- ttr ("PUTO WATCHLIST", wl)


    onAllNodes $ do
      c <- liftIO $ BSS.pack <$> replicateM 5 (randomRIO ('a', 'z'))
      setCookie "session" c
      -- setSession $ BSS.toStrict c
    onAll $ loggedmsg $ "Wallet registered with " ++ show (length $ listPayAddresses useraddr) ++ " payment addresses"
    -- send all available endpoints to the users at any moment
    --  note that <|> return () continues the execution for the initialization thread

    allPublishedEndpoints
    -- minput "next" ("next" ::String) :: Cloud ()
    
    empty



  -- | send all the endpoints for a user. This may vary according with the user state and what other users publish
allp= do
    publish "General" $ minput "allendpts" ("All endpoints for you" :: String) :: Cloud()
    allPublishedEndpoints
    empty

allPublishedEndpoints = local $ do
  UserEnv{userAddress= UserAddress{listStakeAddresses=addr:_}} <- getState
  ttr addr
  output ( str "General")
  published "General"
  output (str "For you")
  published (show addr)
  output (str "Pending")
  published ("Pending" <> show addr)
  where
  str s= s :: String


{-# NOINLINE synchronizing#-}
synchronizing= lazy $ newIORef False

syncProcess = do
  initIndexesDB
  option ("sync" :: String) "sync the cache"
  guard (not $ lazy $ readIORef synchronizing) <|> (liftIO (putStrLn "Already Synchronizing") >> empty)
  liftIO $ writeIORef synchronizing True

  mempoolHyphen <|> syncDB  <|> connectSync
  where
  initIndexesDB = liftIO $ do
    index slotNo
    index owner
    index spent


connectSync= do
    abduce
    env <-  getState <|> error "no connection env"
    ttr "connected"

    liftIO $ do
      atomically $ do
        g <- readDBRef globalSyncState
        when (isNothing g)  $ writeDBRef globalSyncState initSync
      let loop = do
            ttr "RUNSYNC"
            runSyncWithEnv env `catch` \(e :: SomeException) -> do
              putStrLn $ "Node connection lost (" ++ show e ++ "). Reconnecting in 5s..."
              threadDelay 5000000
            loop
      loop

-- | difunde los bloques que llegan en su mailbox de tipo  BlockInMode
-- reactBlockSync= do
--   abduce
--   ev <- getEVarFromMailbox
--   b <- react1 onBlock

--   writeEVar ev (b :: BlockInMode)
--   empty

-- | difunde las transacciones que aparecen en el mempool en su mailbox de tipo TxId
-- reactMempool= do
--   abduce
--   ev <- getEVarFromMailbox
--   t <- react1 onMempoolTx
--   writeEVar ev (t :: TxInMode)
--   empty


-- | stream every block that is generated in the blockchain
blockStream :: TransIO BlockInMode
blockStream=  getMailbox

-- synchronize the cache
syncDB= do
  abduce
  b <- blockStream
  processBlock b
  empty
  where
  processBlock :: BlockInMode -> TransIO ()
  processBlock (BlockInMode era block) = do
    liftIO $ putChar '.'
    case block of
        Block (BlockHeader slot hash _) txs -> do
          threads 5 $ mapM_ (\tx -> do abduce ; liftIO $ processTx slot era tx) txs
          let cp = ChainPoint slot hash
          liftIO $ atomically $ do
            sstate <- readDBRef globalSyncState `Data.TCache.onNothing` error "no sync state"
            writeDBRef globalSyncState $ updateSyncState cp sstate
          when (unSlotNo slot `mod` 100 == 0) $ liftIO $ reaper slot

mempoolSpitter :: TransIO (SlotNo,TxInMode)
mempoolSpitter= do
    MPool x <- getMailbox  
    return x

mempoolHyphen=do
  abduce
  mempoolSpitter
  liftIO $ putChar '-'

-- | Detecta las transacciones propias en el mempool 
processMempoolStreamTracker = do
  abduce
  ev <- getEVarFromMailbox
  (slotNow, TxInMode _era tx) <- mempoolSpitter
  liftIO $ do
    let txId = getTxId (getTxBody tx)
    isMine <- atomically $ Set.member txId <$> readTVar myPendingTxIds
    when isMine $ do
      writeEVar ev (TxInMempool txId slotNow)
      let inputs = txIns $ getTxBodyContent (getTxBody tx)
      mapM_ (markInMempool slotNow) [ txIn | (txIn, _) <- inputs ]
  empty

markInMempool :: SlotNo -> TxIn -> IO ()
markInMempool slot txIn = do
  exists <- existDataForKey (ofType :: MiUTxO) (keyUtxo txIn)
  when exists $ do
    let ref = getDBRef (keyUtxo txIn) :: DBRef MiUTxO
    atomically $ do
      mUtxo <- readDBRef ref
      case mUtxo of
        Just utxo -> writeDBRef ref utxo{ spent = InMempool slot }
        Nothing   -> return ()

{-#NOINLINE initiatedTrackMempool#-}
initiatedTrackMempool= lazy $ newIORef False

-- | Web service: el browser envía un TxId y recibe updates del ciclo de vida
trackTxService :: Cloud ()
trackTxService = do
  minput "track" ("track" :: String) :: Cloud ()
  POSTData (txIdHex :: String) <- minput "txId" ("TxId to track" :: String)
  
  when (not (lazy $ readIORef initiatedTrackMempool)) $ onAll $ do fork processMempoolStreamTracker
  let txId = case (deserialiseFromRawBytesHex (BS.pack txIdHex) :: Either RawBytesHexError TxId) of
               Left err -> error $ "invalid txId: " ++ show err
               Right t  -> t
  moutput $ "Tracking: " ++ txIdHex
  local $ do
    abduce
    event <- getMailbox :: TransIO TxEvent
    case event of
      TxInMempool tid slot | tid == txId ->
          unCloud $ moutput $ "in mempool, slot " ++ show (unSlotNo slot)
      TxConfirmed tid slot | tid == txId ->
          unCloud $ moutput $ "confirmed, slot "  ++ show (unSlotNo slot)
      _ -> empty

-- | inicia los parametros de sincronización
initSync :: SyncState
initSync =
  let slotNum = 102038216
      hashHex = "7f6a694d86692a1e972e219afd0fdd9240acf9075a8519326a932db2046bb452"
  -- let slotNum = 94009695  
  --     hashHex = "6d765545a02fae2350afc649d1eeef2e7760f2ac7393b0045a411094ec10ef19"
      -- Convertimos el texto a bytes y luego a HeaderHash
      rawHash = BS.pack hashHex
      mHash = deserialiseFromRawBytesHex  rawHash
  in case mHash of
    Left err -> error "El Hash no es valido, revisa que este completo"
    Right h  ->
      let point = ChainPoint (SlotNo slotNum) h
      in SyncState [point, ChainPointAtGenesis, ChainPointAtGenesis, ChainPointAtGenesis, ChainPointAtGenesis, ChainPointAtGenesis]

-- | Insert the utxos of a new wallet in the cache
getInitialWalletUtxos userEnv = do
  env <- getState <|>  do liftIO $ print "getInitialWalletUtxos: no wallet set" ; empty
  let addresses  = listPayAddresses $ userAddress env
      ownerCreed = head $ listStakeAddresses $ userAddress env
  ttr ("ownerCreed",ownerCreed)
  isInList <- liftIO $ atomically $ do
        w@(WatchList s _) <- readDBRef watchList `Data.TCache.onNothing` do -- error "getInitialWalletUtxos: no watch list"
                                          let emptywatch = WatchList Set.empty Map.empty
                                          writeDBRef watchList emptywatch
                                          return emptywatch

        -- add to the filtered wallets
        if ownerCreed `Set.member`  credentials w then return True else do
            writeDBRef watchList $ w{credentials = Set.insert ownerCreed s}
            return False


  if isInList  then return () else do
    ttr  "CREATING UTXOS"

    -- appEnv <- ask
    -- (slotNo,utxosNode) <- liftIO $ getUTxOsFromWalletAddressesIO appEnv addresses
    -- utxosCache <- liftIO $ getUtxosFor ownerCreed

    -- -- | Mezcla los UTXOs puros del nodo con tu estructura MiUTxO del caché
    -- let toInsert = getUtxosToInsert slotNo utxosNode utxosCache ownerCreed
    ( _, slotnow) <- currentSlot
    let utxos = walletUtxos userEnv
        toInsert= map (\wu -> let (txin,txout) = unpackUtxo wu in toMiUTxO txin ownerCreed slotnow  txout) utxos :: [MiUTxO]
    ttr ("WALLETUTXOS",utxos)
    liftIO $ withResources [] $ const toInsert

    -- stakeFromAddr :: AddressInEra era ->  StakeCredential
    -- stakeFromAddr (AddressInEra _ (ShelleyAddress _ _ stakeRef)) =
    --   case stakeRef of
    --     StakeAddressByValue cred -> cred
    --     _                        -> error ("stakeFromAddr: invalid stake address")
    -- stakeFromAddr _ = Nothing





    -- where
    -- -- | Obtiene solo los MiUTxO que deben ser insertados (los que no existen en el caché)
    -- getUtxosToInsert :: SlotNo -> UTxO ConwayEra -> [MiUTxO] -> Creed -> [MiUTxO]
    -- getUtxosToInsert slotConsulta utxosNode utxosCache ownerCreed =
    --   let
    --       -- 1. Extraemos los IDs que ya conocemos en el caché (tanto vivos como gastados)
    --       cacheIds = Set.fromList [ utxoId u | u <- utxosCache ]

    --       -- 2. Filtramos los que vienen del nodo que NO están en esos IDs
    --       nuevosDelNodo = filter (\(utxoId, _) -> not (Set.member utxoId cacheIds)) 
    --                              (Map.toList (unUTxO utxosNode))

    --       -- 3. Convertimos solo esos nuevos al formato MiUTxO
    --       toMiUtxo (utxoId, TxOut addrInEra txOutVal _ _) =  
    --           let addrAny = case addrInEra of
    --                   AddressInEra _ a -> toAddressAny a
    --           in MiUTxO 
    --               { utxoId  = utxoId
    --               , addr    = addrAny
    --               , value   = txOutValueToValue txOutVal -- Esta función es la "llave maestra"
    --               , slotNo  = slotConsulta 
    --               , spent   = Nothing  
    --               , owner   = ownerCreed
    --               }

--     in map toMiUtxo nuevosDelNodo


-- -- | Obtiene todos los UTxOs de una lista de direcciones en una sola consulta.
-- getUTxOsFromWalletAddressesIO :: MonadIO m => AppEnv -> [AddressInEra ConwayEra] -> m (SlotNo,UTxO ConwayEra)
-- getUTxOsFromWalletAddressesIO env addresses = liftIO $ do
--   let conn = envConn env
--       -- Forma correcta en cardano-api moderna para AddressInEra -> AddressAny
--       toAddrAny (AddressInEra _ addr) = toAddressAny addr 

--       addressSet = Set.fromList $ map toAddrAny addresses

--   ttr ("getUTxOsFromWalletAddressesIO",length addresses, addresses)

--   let utxoFilter = QueryUTxOByAddress addressSet
--       -- query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway (QueryUTxO utxoFilter)
--       utxoQuery = QueryUTxO (QueryUTxOByAddress addressSet)
--       pointQuery = QueryChainPoint
--       combinedQuery = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway $ 
--                         (,) <$> utxoQuery <*> pointQuery

--   -- currentPoint@(ChainPoint slotNo _) <- chainTipToChainPoint <$> getLocalChainTip (envConn env)
--   -- result <- runExceptT $ queryNodeLocalState conn VolatileTip query
--   result <- runExceptT $ queryNodeLocalState conn VolatileTip combinedQuery
--   ttr ("after getUTxOsFromWalletAddressesIO")
--   case result of
--     Left eb -> do print eb; getUTxOsFromWalletAddressesIO env addresses
--     Right (Left e) -> error $ show e
--     Right (Right (utxos, currentPoint@(ChainPoint slotNo _))) -> do
--        valid <- isPointValid conn (SpecificPoint currentPoint)
--        if valid then return (slotNo,utxos) else do print "rollback happened"; threadDelay 1000000; getUTxOsFromWalletAddressesIO env addresses

--   where
--   -- isPointValid :: AppEnv -> ChainPoint -> IO Bool
--   isPointValid conn point = do

--     -- Intentamos hacer una consulta mínima (como pedir el Slot actual) 
--     -- apuntando específicamente a ese ChainPoint.
--     -- Si el punto ha sufrido un rollback, 'queryNodeLocalState' fallará 
--     -- antes de ejecutar la query interna.
--     result <- runExceptT $ queryNodeLocalState conn point QueryChainPoint

--     case result of
--       -- Si el nodo acepta el punto, es que sigue en la cadena principal
--       Right _ -> return True

--       -- Si hay un error de "PointNotFound" o similar, el punto es inválido
--       Left err -> do
--         putStrLn $ "Punto inválido detectado (posible rollback): " ++ show err
--         return False



collect :: Loggable a => Int -> Int -> Cloud a -> Cloud [a]
collect =Transient.Move.Job.collectc

-- runAt

-- teleport

-- sync


