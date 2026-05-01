{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

-- for pipelined @10
-- {-# LANGUAGE DataKinds, KindSignatures, GADTSyntax #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module CardanoC.Sync where

import Cardano.Api hiding(onNothing)
import Ouroboros.Network.Protocol.ChainSync.Client
  -- ( ChainSyncClient(  )
  -- , ClientStIdle(  )
  -- , ClientStNext(  )
  -- , ClientStIntersect(  )
  -- )

-- Los mensajes del protocolo (como SendMsgFindIntersect) están aquí
import Ouroboros.Network.Protocol.ChainSync.Type
  ( Message(  ) )

import Ouroboros.Network.Protocol.LocalTxMonitor.Client as Monitor

-- import Cardano.Ledger.Credential

-- for chainSyncPipelined  
-- import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as ClientP
-- --   -- ( ChainSyncClientPipelined(  )
-- --   -- , ClientPipelinedStIdle(  ), SendMsgRequestNextPipelined
-- --   -- , ClientStNext(  )
-- --   -- , PipelineDecision(  ), PipelineContinue, PipelineStop
-- --   -- )
-- import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSyncP
-- --   -- ( SendMsgFindIntersect
-- --   -- , ClientStIntersect(  )
-- --   -- )
-- import GHC.TypeLits -- (KnownNat, natVal, Proxy(  ))

--- END pipelined

import Data.Typeable
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import GHC.Generics ( Generic )
import Transient.Base 
import Data.TCache 
import Data.TCache.DefaultPersistence
import Data.TCache.Defs
import Data.TCache.IndexQuery
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Transient.Loggable(fragment)
import CardanoC.Defs
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSS

import Data.Coerce
import Data.Maybe
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.List as List

import Debug.Trace

import Data.Aeson hiding (Value)
import qualified Data.Aeson(Value)
import qualified Text.Parsec as Parsec
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent(threadDelay)

-- | Synchronization state to persist the exact point
data SyncState = SyncState
  {  points :: [ChainPoint]
  } deriving(Generic,FromJSON,ToJSON) -- (Show, Read, Typeable)

instance Serializable SyncState where
      serialize= encode
      deserialize s= fromMaybe (error $ "syncState: deserialize decoding:"<> BSS.unpack s) $ decode s

-- instance {-# OVERLAPPING #-}(FromJSON a,ToJSON a)=> Serializable a where
--   serialize= encode
--   deserialize s= fromMaybe (error $"deserialize decoding:"<> BSS.unpack s) $ decode s



instance Indexable SyncState where
  key _ = "global_sync_state"
  defPath _= "syncDB/"

globalSyncState= getDBRef "global_sync_state" :: DBRef SyncState



-- deriving instance Generic PaymentKey
-- deriving instance FromJSONKey PaymentKey
-- deriving instance FromJSON PaymentKey
-- deriving instance ToJSONKey  PaymentKey
-- deriving instance ToJSON  PaymentKey

-- deriving instance Ord PaymentKey
-- deriving instance Eq PaymentKey

-- -- | Instancia manual para serializar un Hash a una clave de texto hexadecimal en JSON.
-- instance ToJSONKey (Hash PaymentKey) where
--   toJSONKey = toJSONKeyText (serialiseToRawBytesHexText)

---- | Instancia manual para deserializar una clave de texto hexadecimal a un Hash.
----   Esto es necesario para que `FromJSON` funcione con `Map (Hash PaymentKey) v`.
-- instance FromJSONKey (Hash PaymentKey) where
--   fromJSONKey = FromJSONKeyTextParser $ \t ->
--     case deserialiseFromRawBytesHex  (T.encodeUtf8 t) of
--       Right h  -> pure h
--       Left err -> fail $ "No se pudo deserializar la clave de pago del hash: " ++ show err

-- MOVED TO DEFS
-- data WatchList = WatchList 
--   { credentials   :: Set.Set Creed  
--   , vPaymentKeys :: Map.Map (Hash PaymentKey) Creed 
--   } deriving (Generic,FromJSON,ToJSON) 

-- -- instance Serializable WatchList where
-- --     serialize  = BSS.pack . show
-- --     deserialize= read . BSS.unpack

-- instance  Serializable WatchList where
--   serialize= encode
--   deserialize s= fromMaybe (error $"deserialize decoding:"<> BSS.unpack s) $ decode s


-- instance Indexable WatchList where
--   key _ = "app_watch_list" -- Clave única para encontrar el archivo
--   defPath _= ".tcachedata/syncDB/"

-- watchList= getDBRef "app_watch_list" :: DBRef WatchList


deriving instance Generic AddressAny
deriving instance FromJSON AddressAny
deriving instance ToJSON AddressAny

-- -- | Representación de un UTxO en nuestra DB local
-- data MiUTxO = MiUTxO 
--   { utxoId   :: TxIn
--   , addr     :: AddressAny -- Guardamos la dirección completa para facilidad
--   , value    :: Value
--   , slotNo   :: SlotNo          -- creation block
--   , spent    :: Maybe SlotNo    -- spent block   UTxOStatus 
--   , owner    :: Creed
--   }  -- (Show, Read, Typeable)

-- -- data UTxOStatus 
-- --   = Unspent                -- Confirmado en Ledger (Bloque)
-- --   | InMempool SlotNo       -- Intento de gasto o recepción (Radar)
-- --   | Spent SlotNo           -- Gastado y confirmado (Bloque)
-- --   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

reaper :: SlotNo -> IO ()
reaper currentSlot = atomically $ do
  -- Restore InMempool UTxOs older than 20 slots back to Unspent (tx dropped from mempool)
  expiredRefs <- spent .>. Unspent .&&. spent .<. InMempool (currentSlot - 20)
  mapM_ restoreToUnspent expiredRefs
  -- Delete only confirmed-Spent UTxOs older than 2160 slots (36 min)
  -- Using Spent (SlotNo 0) as lower bound to avoid accidentally matching InMempool entries
  oldSpentRefs <- spent .>=. Spent (SlotNo 0) .&&. spent .<. Spent (currentSlot - 2160)
  mapM_ delDBRef oldSpentRefs
  where
  restoreToUnspent ref = do
    utxo <- readDBRef ref `Data.TCache.onNothing` error (show ("reaper: not found utxo:", ref))
    writeDBRef ref utxo{ spent = Unspent }


-- instance ToJSON MiUTxO where
--   toJSON (MiUTxO uId ad val sl sp ow) = object
--     [ "utxoId" .= renderTxIn uId
--     , "addr"   .= serialiseAddress ad
--     , "value"  .= valueToJSON val
--     , "slotNo" .= sl
--     , "spent"  .= sp
--     , "owner"  .= ow
--     ]

-- instance FromJSON MiUTxO where
--   parseJSON = withObject "MiUTxO" $ \v -> do
--     uIdStr <- v .: "utxoId"
--     -- Corregido: Ejecutamos el parser sobre el texto
--     uId <- case Parsec.runParser parseTxIn () "" uIdStr of
--              Right tid -> return tid
--              Left err  -> fail $ "ID de UTxO inválido: " ++ show err

--     adStr <- v .: "addr"
--     ad <- case deserialiseAddress AsAddressAny adStr of
--             Just a  -> return a
--             Nothing -> fail "Dirección inválida"

--     MiUTxO uId ad 
--       <$> v .: "value"
--       <*> v .: "slotNo"
--       <*> v .: "spent"
--       <*> v .: "owner"

-- -- Función corregida para cardano-api 10.19.1.0
-- valueToJSON :: Value -> Data.Aeson.Value
-- valueToJSON v = toJSON $ valueToNestedRep v

-- emptyUtxo= let u= undefined in MiUTxO u u u u u u

-- instance Serializable MiUTxO where
--   serialize= encode
--   deserialize s= fromMaybe (error $"deserialize decoding:  "<> BSS.unpack s) $ decode s


-- instance Read SlotNo where
--   readsPrec d s = readParen (d > 10) (\r -> do
--     ("SlotNo", rest) <- lex r
--     (n, next)        <- readsPrec 11 rest
--     return (SlotNo n, next)) s

-- -- deriving instance FromJSON SlotNo 

-- -- instance Show Addr where
-- --   show (Addr a)= T.unpack $ serialiseAddress a

-- -- instance Read Addr where
-- --   readsPrec _ ss =
-- --     let (s,rest)= fragment $ BSS.pack ss
-- --         mr = coerce $ deserialiseAddress (asType :: AsType AddressAny) $ T.decodeUtf8 $ BSS.toStrict s
-- --     in if isJust mr then [(fromJust mr, BSS.unpack rest)] else []

-- -- El ID del UTxO (TxHash#Index) es la clave única
-- instance Indexable MiUTxO where
--   key u = keyUtxo (utxoId u)
--   defPath _= ".tcachedata/syncDB/utxos/"

-- keyUtxo (TxIn txId (TxIx index))= T.unpack(serialiseToRawBytesHexText txId) ++ "#" ++ show index

-- | Retrieves all UTxOs for a specific staking credential
getUtxosFor :: Creed -> IO [MiUTxO]
getUtxosFor cred = atomically $ recordsWith $ owner .==. cred .&&. spent .==. Unspent



-- -- moved to SF.hs
-- processBlock :: BlockInMode -> IO ()
-- processBlock (BlockInMode era block) = do
--   case block of
--         Block (BlockHeader slot@(SlotNo s) hash _) txs -> do
--           mapM_ (processTx slot era) txs
--           let cp = ChainPoint slot hash
--           atomically $ do
--             sstate <- readDBRef globalSyncState `Data.TCache.onNothing` error "no sync state"
--             writeDBRef globalSyncState $ updateSyncState cp sstate
--           when (s `mod` 100 == 0) $ reaper slot

--     -- We ignore any other era (Byron, Babbage, etc.) to avoid type errors
--     -- _ -> putStrLn $ "Ignoring block of era: " ++ show era -- return ()


analyzeOutput :: WatchList -> TxId -> SlotNo -> (Int, TxOut CtxTx era) -> IO ()
analyzeOutput wl txId slot (idx, TxOut addrInEra value txOutDatum refScriptField) = do
  let addrAny = case addrInEra of AddressInEra _ a -> {-Addr-} (toAddressAny a)

  case addrInEra of
    AddressInEra _ (ShelleyAddress _ p s) -> do
      let stakeRef = fromShelleyStakeReference s
      let payRef   = fromShelleyPaymentCredential p
      let pkh      = extractPaymentKeyHash payRef

      -- LOOK FOR THE OWNER (as Creed)
      -- We try to extract it from the address or from the vPaymentKeys map
      let mOwner = case stakeRef of
            -- Case A: Base Address. We wrap the StakeCredential in Creed
            StakeAddressByValue sc -> Just (Creed sc)
            -- Case B: Enterprise. The Map already returns a Creed directly
            _ -> Map.lookup ({- HashC -} pkh) (vPaymentKeys wl)

      -- Now we operate on the found Creed
      case mOwner of
        Just ownerCreed -> do
          -- ownerCreed is already of type Creed, we can compare it directly with the Set
          -- print(ownerCreed,credentials wl)
          when (Set.member  ownerCreed (credentials wl)) $ do
              putStrLn $ "UTxO Found! Owner: " ++ show ownerCreed

              let utxo = MiUTxO {
                    utxoId = TxIn txId (TxIx (fromIntegral idx)),
                    addr   = addrAny,
                    value  = txOutValueToValue value,
                    datum     = extractDatum txOutDatum,
                    datumHash = extractDatumHash txOutDatum,
                    refScript = extractRefScript refScriptField,
                    slotNo = slot,
                    owner  = ownerCreed, -- <--- Ya es el tipo Creed que esperabas
                    -- owner  = ownerCreed, -- <--- It's already the Creed type you expected
                    spent  = Unspent
                  }

              withResources [] $ const [utxo]

        Nothing -> return ()
    _ -> return ()
  where
    extractDatum :: TxOutDatum CtxTx era -> Maybe HashableScriptData
    extractDatum d = case d of
      TxOutDatumNone              -> Nothing
      TxOutDatumHash _ _          -> Nothing
      TxOutSupplementalDatum _ hs -> Just hs
      TxOutDatumInline _ hs       -> Just hs

    extractDatumHash :: TxOutDatum CtxTx era -> Maybe (Hash ScriptData)
    extractDatumHash d = case d of
      TxOutDatumHash _ h -> Just h
      _                  -> Nothing

    extractRefScript :: ReferenceScript era -> Maybe ScriptInAnyLang
    extractRefScript rs = case rs of
      ReferenceScriptNone    -> Nothing
      ReferenceScript _ sAny -> Just sAny




{- |
  Forcibly removes network prefixes (like '0xe0') by extracting the raw 
  ledger-level hash from a 'StakeCredential'.
  
  **Context:**
  Standard serialization functions might preserve network headers if the 
  credential was originally parsed from a Mainnet address. By unwrapping 
  the 'StakeKeyHash' to its ledger-representative form, we ensure 
  the resulting credential is stripped of all non-essential metadata.
-}
normalizeStakeCred :: Creed -> Creed
normalizeStakeCred (Creed cred) = Creed $ case cred of
    StakeCredentialByKey (StakeKeyHash ledgerHash) ->
        -- We re-pack the ledger hash directly.
        -- This ensures that any previous serialization header is ignored.
        StakeCredentialByKey (StakeKeyHash ledgerHash)

    StakeCredentialByScript (ScriptHash ledgerHash) ->
        StakeCredentialByScript (ScriptHash ledgerHash)

analyzeInputs :: SlotNo -> TxIn -> IO ()
analyzeInputs currentSlot txIn = do
  exists <- existDataForKey (ofType :: MiUTxO) (keyUtxo txIn)
  when exists $ do
    let ref = getDBRef (keyUtxo txIn) :: DBRef MiUTxO
    mUtxo <- atomically $ readDBRef ref
    case mUtxo of
      Just utxo -> do
        atomically $ writeDBRef ref utxo{ spent = Spent currentSlot }
        putStrLn $ "UTxO Spent at slot " ++ show currentSlot ++ ": " ++ show txIn
      Nothing -> return ()

processTx :: SlotNo -> CardanoEra e -> Tx e -> IO ()
processTx slot era tx = do
  let txBody  = getTxBody tx
      content = getTxBodyContent txBody
      inputs  = txIns content
      outputs = txOuts content
      txId = getTxId txBody
  wl@(WatchList _  _) <- atomically $ readDBRef watchList `Data.TCache.onNothing` return (WatchList Set.empty Map.empty)

  mapM_ (analyzeInputs slot) [ txIn | (txIn, _) <- inputs ]
  mapM_ (analyzeOutput wl txId slot) (zip [0..] outputs)
  atomically $ untrackTx txId
  putMailbox (TxConfirmed txId slot)






handleRollback :: ChainPoint -> IO ()
handleRollback targetPoint = atomically $ do
  (invalidUtxosRefs,spentUtxosRefs)  <-
        case targetPoint of

          ChainPointAtGenesis -> do
                  regs <- indexOf slotNo :: STM[(SlotNo, [DBRef MiUTxO])]
                  let rs= mconcat $ map snd  regs
                  return (rs,[])

          -- All the utxos created after the target point must be deleted.
          -- All the utxos created before the target point and spent after the target point should be unspent
          ChainPoint slot _ -> (,) <$> (slotNo .>. slot) <*> (slotNo .<. slot .&&. spent .>. Spent slot)
  -- delete all the utxos created after the target point
  unsafeIOToSTM $ putStrLn $ "Deleting " <> show (length invalidUtxosRefs) <> " invalid utxos" <> " ," <> show (length spentUtxosRefs) <> " spent utxos"
  mapM_ delDBRef invalidUtxosRefs
  -- mark as unspent the utxos spent after the target point
  mapM_ setunexpent spentUtxosRefs


  -- We update the synchronization state
  sstate <- readDBRef globalSyncState `Data.TCache.onNothing` error "no sync state"
  writeDBRef globalSyncState $ updateSyncState targetPoint sstate
  where
  setunexpent dbr= do
    reg <- readDBRef dbr `Data.TCache.onNothing` error ("handleRolback: not found UTxO: " <> show dbr)
    writeDBRef dbr $ reg{spent= Unspent}


  -- update the six checkpoints each 1, 10, 1000, 10000 . blocks
updateSyncState :: ChainPoint -> SyncState -> SyncState
updateSyncState newPoint (SyncState oldPoints) =
      SyncState $ map updateByIndex (zip [0..5] oldPoints)
    where
      updateByIndex (i, oldP) =
          let frequency = 10 ^ i
              s = getSlotNum newPoint
          in if s `mod` frequency == 0
            then newPoint
            else oldP
      getSlotNum :: ChainPoint -> Word64
      getSlotNum ChainPointAtGenesis = 0
      getSlotNum (ChainPoint (SlotNo s) _) = s


-- {-# NOINLINE onBlockRef #-}
-- onBlockRef= unsafePerformIO $ newIORef Nothing

-- onBlock f= writeIORef onBlockRef $ Just f

-- {-# NOINLINE processBlockHandler #-}
-- processBlockHandler= unsafePerformIO $ do
--         r <- readIORef onBlockRef
--         case r of
--           Nothing -> error "Please use runCC to initialize the CardanoC program, or 'react1 onBlock' to set the handler of the block processing "
--           Just f -> return f

chainSyncFilterClient :: IO (ChainSyncClient BlockInMode ChainPoint ChainTip IO ())
chainSyncFilterClient = do
  ev <- getEVarFromMailbox

  SyncState resumePoints <- atomically $ readDBRef globalSyncState `Data.TCache.onNothing`  error "no sync state"

  -- Define the states recursively first
  let clientStNext = ClientStNext
        { recvMsgRollForward  = \blk _tip ->
            ChainSyncClient $ do writeEVar ev blk; pure clientStIdle
        , recvMsgRollBackward = \point _tip ->  ChainSyncClient $ do
            let ChainPoint (SlotNo s) _ =  point
                ChainTip (SlotNo s') _ _ =  _tip
            putChar '\n'
            print ("rollback slots:", toInteger s' - toInteger s)
            handleRollback point; pure clientStIdle
        }

      clientStIdle = SendMsgRequestNext (return ()) clientStNext

      clientStIntersect = ClientStIntersect
        { recvMsgIntersectFound = \point _tip -> ChainSyncClient $ do
            -- IMPORTANT: If 'point' < 'resumePoint', we have to clean
            -- our DB before requesting new blocks.
            let ChainPoint (SlotNo s) _ =  point
                ChainTip (SlotNo s') _ _ =  _tip
            putChar '\n'
            print ("rollback slots:", toInteger s' - toInteger s)
            handleRollback point
            pure clientStIdle

        , recvMsgIntersectNotFound = \_tip -> ChainSyncClient $ do
            -- If the intersection is not found, the node will send us to Genesis.
            -- We must delete ABSOLUTELY EVERYTHING.
            let ChainTip (SlotNo s') _ _ =  _tip
            print ("rollback to ChainPointAtGenesis. Slots:", toInteger s')
            handleRollback ChainPointAtGenesis
            pure clientStIdle
        }

  let client = ChainSyncClient $
        pure $ SendMsgFindIntersect (resumePoints <> [ChainPointAtGenesis]) clientStIntersect

  pure client


-- chainSyncPipelinedClient :: forall (depth :: Nat). KnownNat depth 
--                         => IO (ClientP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ())
-- chainSyncPipelinedClient = do
--   maybeState <- atomically $ readDBRef globalSyncState
--   let resumePoint = case maybeState of
--         Nothing -> ChainPointAtGenesis
--         Just (SyncState p) -> p

--   let clientStNext = ClientP.ClientStNext
--         { ClientP.recvMsgRollForward  = \blk _tip -> do
--             processBlock blk
--             pure ClientP.PipelineContinue
--         , ClientP.recvMsgRollBackward = \point _tip -> do
--             handleRollback point
--             pure ClientP.PipelineStop
--         }

--       clientStIdle = ClientP.SendMsgRequestNextPipelined
--         (do
--           atomically $ modifyDBRef globalSyncState (Just . SyncState currentPoint)
--           putStrLn $ "Pipelined batch @" ++ show (natVal (Proxy :: Proxy depth))
--         ) clientStNext

--       clientStIntersect = ChainSyncP.ClientStIntersect
--         { ChainSyncP.recvMsgIntersectFound    = \_ _ -> pure clientStIdle
--         , ChainSyncP.recvMsgIntersectNotFound = \_   -> pure clientStIdle
--         }

--   pure $ ClientP.ChainSyncClientPipelined $
--     pure $ ChainSyncP.SendMsgFindIntersect [resumePoint] clientStIntersect

-- Pipelined usage (depth=10):
-- withLocalNodeConnPipelined localNodeConnParams (chainSyncPipelinedClient @10)   .


-- | Executes the synchronization client using a pre-configured application environment
runSyncWithEnv :: AppEnv -> IO ()
runSyncWithEnv env = do
    -- 1. We get the synchronization client with its TCache logic

    client <- chainSyncFilterClient
    -- 2. We execute the connection using the environment information
    putStrLn "Connecting to the node and synchronizing with TCache  ."
    connectToLocalNode (envConn env) $
      LocalNodeClientProtocols
        { localChainSyncClient    = LocalChainSyncClient client
        , localTxSubmissionClient = Nothing
        , localStateQueryClient   = Nothing
        , localTxMonitoringClient = Just monitorMempoolProc
        }

extractPaymentKeyHash :: PaymentCredential ->  Hash  PaymentKey
extractPaymentKeyHash cred = case cred of
    -- The trick here is to use the PaymentKeyHash constructor from the API
    -- to wrap the 'kh' hash that comes from the Ledger.
    PaymentCredentialByKey kh ->
      -- In 10.19, PaymentKeyHash is directly the alias we need
      kh

    PaymentCredentialByScript sh ->
      -- For scripts, we convert the ScriptHash to PaymentKeyHash via bytes
      -- But using the method from the internal crypto library that the Ledger already knows
      case deserialiseFromRawBytes (AsHash AsPaymentKey) (serialiseToRawBytes sh) of
        Right h -> h
        Left err -> error $ "Error en ScriptHash: " ++ show err





{- |
  Updates the 'WatchList' by associating a payment key hash with a 'Creed' 
  (StakeCredential) if the provided address is an Enterprise address.

  **Context:**
  In Cardano, Enterprise addresses do not contain a stake identifier. To track 
  UTxOs for these addresses under a specific user's 'Creed', we maintain a 
  mapping ('vPaymentKeys') in the 'WatchList'.

  **Logic:**
  1. Inspects the 'AddressInEra'.
  2. If it is a Shelley-style 'NoStakeAddress' (Enterprise):
     - Calculates the 'PaymentKeyHash'.
     - Searches the provided '[UserEnv]' to find which user owns this address.
     - If an owner is found, it inserts the (PaymentKeyHash -> Creed) pair 
       into the 'vPaymentKeys' map.
  3. If it is a Base address or non-Shelley address, it returns the 
     'WatchList' unchanged as the stake credential is already present 
     in the address itself.

  **Parameters:**
  - 'WatchList': The current state of watched credentials and key mappings.
  - '[UserEnv]': The list of known users and their associated address sets.
  - 'AddressInEra ConwayEra': The specific address to analyze and potentially map.
-}
watchAddr :: MonadIO m => UserEnv -> [AddressInEra ConwayEra] -> m ()
watchAddr env addr= liftIO $ do
  print "WATCH"

  atomically $ do
    wl <- readDBRef watchList `Data.TCache.onNothing` return (WatchList Set.empty Map.empty)
    let wl' = updateWL wl addr
    writeDBRef watchList wl'
  print "WATCH DONE"
  where

  updateWL :: WatchList -> [AddressInEra ConwayEra] -> WatchList
  updateWL wl txAddrs =
    -- We extract the user's first StakeCredential (their identity)
    case listStakeAddresses (userAddress env) of
      [] -> traceShow "NO STAKE ADDRESS"  wl -- Si el usuario no tiene Stake Key, no podemos mapear nada
      -- If the user has no Stake Key, we can't map anything
      (s:_) ->
        let
          ownerCreed = traceShow ("STAKE",s)  s
          wlWithStake = wl { credentials = Set.insert ownerCreed (credentials wl) }
          -- We get all the user's known addresses to compare
          userKnownAddrs =traceShow ("ADDRESSES",getUserAddrs env,addr ) $  getUserAddrs env
        in
          -- We filter the TX addresses that belong to this user
          let userTxAddrs = [ a | a <- txAddrs, a `elem` userKnownAddrs ]
          in List.foldl' (insertIfEnterprise  ownerCreed) wlWithStake userTxAddrs

    where
      -- Internal function to insert only if it's Enterprise
      insertIfEnterprise :: Creed -> WatchList -> AddressInEra ConwayEra -> WatchList
      insertIfEnterprise creed currentWL addr =
        case addr of
          AddressInEra _ (ShelleyAddress _ p s) ->
            let
              stakeRef = fromShelleyStakeReference s
              payRef   = fromShelleyPaymentCredential p
              pkh      = extractPaymentKeyHash payRef
            in
              case stakeRef of
                -- We only map if it's Enterprise (NoStake)
                NoStakeAddress ->
                  currentWL { vPaymentKeys = Map.insert ({-HashC-} pkh) creed (vPaymentKeys currentWL) }
                _ -> currentWL -- If it's Base, it's already identified by its stake
          _ -> currentWL

      -- Helper to get all user addresses
      getUserAddrs :: UserEnv -> [AddressInEra ConwayEra]
      getUserAddrs u =
        let ua = userAddress u
        in listPayAddresses ua ++ listUnusedAddresses ua ++ maybeToList (changeAddress ua)



{- |
  Registers a standalone Enterprise address (e.g., created via cardano-cli) 
  into the 'WatchList' by generating a deterministic 'Creed' from its 
  Payment Key Hash (PKH). 
  
  Since the API enforces strict separation between Payment and Stake keys, 
  this function bypasses the type system by serializing the PKH to raw bytes 
  and re-deserializing them as a 'StakeKeyHash'.
-}
watchRawAddress :: MonadIO m => AddressInEra ConwayEra -> m ()
watchRawAddress addr = liftIO $ do
  case addr of
    AddressInEra _ (ShelleyAddress _ p _) -> do
      let apiPayRef = fromShelleyPaymentCredential p
          pkh = extractPaymentKeyHash apiPayRef
          pkhBytes = serialiseToRawBytes pkh

      -- In this API version, deserialiseFromRawBytes returns Either
      case deserialiseFromRawBytes (AsHash AsStakeKey) pkhBytes of
        Left err ->
          putStrLn $ "Error converting PKH to StakeHash: " ++ show err

        Right stakeKeyHash -> do
          let ownerCreed = Creed (StakeCredentialByKey stakeKeyHash)

          atomically $ do
            wl <- readDBRef watchList
                  `onNothing` return (WatchList Set.empty Map.empty)

            let updatedWL = wl {
                  credentials = Set.insert ownerCreed (credentials wl),
                  vPaymentKeys = Map.insert ({-HashC -} pkh) ownerCreed (vPaymentKeys wl)
                }
            writeDBRef watchList updatedWL

          putStrLn $ "Standalone address registered. PKH (as Creed): " ++ show pkh

    _ -> putStrLn "Error: Only Shelley/Enterprise addresses are supported."



-- | Calculates the total balance in Lovelace for a specific owner using the synchronized DB
calculateBalance :: Creed -> IO Integer
calculateBalance targetOwner = do

    activeUtxos <- getUtxosFor targetOwner

    -- In Cardano, AdaAssetId identifies Lovelace within a Value
    let total = List.foldl' (\acc u -> acc + getValueLovelace (value u)) 0 activeUtxos

    return total

-- | Extracts the amount of Lovelace from a Value without using the 'Lovelace' constructor
getValueLovelace :: Value -> Integer
getValueLovelace v =
    -- selectAsset returns a Quantity, which is a simple Integer underneath
    let (Quantity q) = selectAsset v AdaAssetId
    in q



-- onMempoolTxRef :: IORef (Maybe a)
-- onMempoolTxRef = unsafePerformIO $ newIORef Nothing

-- onMempoolTx f= writeIORef onMempoolTxRef $ Just f

-- {-# NOINLINE mempoolTxHandler #-}
-- mempoolTxHandler= unsafePerformIO $ do
--    r <- readIORef onMempoolTxRef
--    case r of
--     Nothing -> error "Please use runCC to init the cardanoC computation or use 'react1 onMempoolTx' to init the mempool handler"
--     Just f -> return f

-- TxIdInMode: The TX ID wrapped for the mode (CardanoMode)
-- TxInMode: The wrapped era (we don't fix Conway here anymore)
-- SlotNo: The result of the acquisition
-- The correct order is: (TX ID) (The Era) (Result)
-- 1. We assume your IORef now stores a callback that accepts TxInMode
-- onMempoolTxRef :: IORef (Maybe (TxInMode -> IO ()))



monitorMempoolProc :: LocalTxMonitorClient TxIdInMode TxInMode SlotNo IO ()
monitorMempoolProc = LocalTxMonitorClient $ do
  ev <- getEVarFromMailbox
  return $ acquireLoop ev
  where
    acquireLoop ev = Monitor.SendMsgAcquire $ \slotNo -> return $ queryNextTx ev slotNo

    queryNextTx ev slotNo = Monitor.SendMsgNextTx $ \mNextTx -> case mNextTx of
      Nothing -> do
        threadDelay 100000
        return $ Monitor.SendMsgRelease (return $ acquireLoop ev)

      Just txInMode -> do
        -- putStrLn "\n!!! [MEMPOOL] TRANSACCIÓN DETECTADA !!!"

        writeEVar ev $ MPool(slotNo, txInMode)
        return $ queryNextTx ev slotNo