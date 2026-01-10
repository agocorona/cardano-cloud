{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module CardanoC.Api where

import Cardano.Api
import Cardano.Api.HasTypeProxy (AsType(..))
import Cardano.Api.Ledger(PParams)
import Cardano.Api.Query  -- (QueryEraHistory(..))




import Transient.Base
import Transient.Move
import Transient.Move.Web
import Transient.Parse


import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import qualified Data.Set as Set
import Data.Map as Map
import Control.Monad 
import Control.Concurrent(threadDelay)

import Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import Data.ByteString.Builder

import GHC.Generics

import Data.Typeable
import Data.Word (Word8)

data CloudEnv = CloudEnv
  { envConn       :: LocalNodeConnectInfo
  , envSigningKey :: SigningKey PaymentExtendedKey
  , envOwnAddress :: AddressInEra ConwayEra
  , envPParams    :: PParams (ShelleyLedgerEra ConwayEra)  -- Cambio clave aquí
  , envNetworkId  :: NetworkId
  , envEra        :: AnyCardanoEra
  }

initCloudEnv 
  :: FilePath
  -> NetworkId
  -> FilePath
  -> IO CloudEnv
initCloudEnv socketPath networkId skeyPath = do
  -- Cargar signing key
  eSKey <- readFileTextEnvelope  (File skeyPath)
  skey <- case eSKey of
    Left err  -> error $ "Error leyendo skey: " ++ show err
    Right k   -> return k

  -- Derivar own address
  let vkey = castVerificationKey (getVerificationKey skey) :: VerificationKey PaymentKey
      pkh  = verificationKeyHash vkey
      ownAddr = makeShelleyAddressInEra ShelleyBasedEraConway networkId
                  (PaymentCredentialByKey pkh)
                  NoStakeAddress

  -- Conexión local
  let conn = LocalNodeConnectInfo
               { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
               , localNodeNetworkId       = networkId
               , localNodeSocketPath      = File socketPath
               }

  -- Query PParams (forma actual en Conway)
  eResult <- runExceptT $ queryNodeLocalState conn VolatileTip
                        (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters))

  pparams <- case eResult of
               Left acquiringErr -> error $ "Fallo adquiriendo conexión al nodo: " ++ show acquiringErr
               Right (Left eraMismatch) -> error $ "Mismatch de era en query de PParams: " ++ show eraMismatch
               Right (Right pp) -> return pp  -- pp :: PParams ConwayEra (del ledger, coincide con tu envPParams)
  let currentEra = AnyCardanoEra ConwayEra

  putStrLn "CloudEnv inicializado correctamente!"
  putStrLn $ "Network: " ++ show networkId
  putStrLn $ "Own address: " ++ show(serialiseAddress ownAddr)
  putStrLn $ "Socket: " ++ socketPath

  return CloudEnv
    { envConn       = conn
    , envSigningKey = skey
    , envOwnAddress = ownAddr
    , envPParams    = pparams
    , envNetworkId  = networkId
    , envEra        = currentEra
    }


ask= getState <|> error ("cardano cloud: no state") :: TransIO CloudEnv

-- ===========================================================================
-- Primitivas clave usando la conexión reutilizada
-- ===========================================================================

-- 6. currentSlot → Usa directamente getLocalChainTip con envConn
currentSlot :: TransIO SlotNo
currentSlot = do
  env <- ask
  tip <- liftIO $ getLocalChainTip (envConn env)
  return $ case tip of
    ChainTipAtGenesis -> SlotNo 0
    ChainTip slotNo _hash _blockNo -> slotNo

-- 4. waitUntil → Polling eficiente usando la misma conexión
waitUntil :: SlotNo -> Cloud ()
waitUntil targetSlot = local $ do
  env <- ask
  let conn = envConn env
      loop = do
        tip <- getLocalChainTip conn
        let current = case tip of
              ChainTipAtGenesis -> SlotNo 0
              ChainTip s _ _ -> s
        if current >= targetSlot
          then return ()
          else do
            liftIO $ threadDelay 1_000_000 -- 1 segundo (ajustable)
            loop
  void loop

-- Alternativa: wait por duración en segundos (más amigable)
waitSeconds :: Int -> Cloud ()
waitSeconds secs = localIO $ threadDelay (secs * 1_000_000)


-- 5. getUTxOsAt → Usa queryNodeLocalState con la conexión existente
getUTxOsAtIO :: CloudEnv -> AddressInEra ConwayEra -> IO (UTxO ConwayEra)
getUTxOsAtIO env addr = do
  let conn = envConn env
      addrAny = case deserialiseAddress AsAddressAny (serialiseAddress addr) of
                  Nothing   -> error "Invalid address conversion"
                  Just any  -> any
      -- query :: QueryInMode CardanoMode (Either LedgerQueryFailure (UTxO ConwayEra))
      query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway
                     (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny)))

  result <- liftIO $ runExceptT  $ queryNodeLocalState conn VolatileTip  query
  case result of
    Left acquireFail -> error $ "Acquire failed: " ++ show acquireFail
    Right (Left e)   -> error $ "Acquire failed: " ++ show e
    Right (Right utxo)       -> return utxo

-- Construye y balancea, pero NO firma ni envía

buildAndBalanceTxIO :: CloudEnv -> TxBodyContent BuildTx ConwayEra -> IO (BalancedTxBody ConwayEra)
buildAndBalanceTxIO env bodyContent = do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env  -- PParams (ShelleyLedgerEra ConwayEra)

  utxo <- getUTxOsAtIO env ownAddr

  systemStart <- runExceptT (queryNodeLocalState conn VolatileTip QuerySystemStart)
                 >>= either (error . show) return

  eraHistory <- runExceptT (queryNodeLocalState conn VolatileTip QueryEraHistory)
                >>= either (error . show) return

  let shelleyBasedEra = ShelleyBasedEraConway

      epochInfo = toLedgerEpochInfo eraHistory

      ledgerPParams = LedgerProtocolParameters rawPParams

      stakePools :: Set.Set PoolId
      stakePools = Set.empty

      depositReturns :: Map.Map StakeCredential Coin
      depositReturns = Map.empty

      -- Parámetro para retornos de depósitos DRep (vacío si no deregistras DReps)
      drepReturns :: Map.Map a Coin
      drepReturns = Map.empty

  let autoBalance = makeTransactionBodyAutoBalance
                      shelleyBasedEra
                      systemStart
                      epochInfo
                      ledgerPParams
                      stakePools
                      depositReturns
                      drepReturns  -- ← Aquí va el "undefined" tipado como Map.empty
                      utxo
                      bodyContent

      balancedEither = autoBalance ownAddr Nothing

  either (\err -> error $ "Error al balancear tx: " ++ show err)
         return
         balancedEither


signTxServerIO :: CloudEnv -> TxBody ConwayEra -> IO (Tx ConwayEra)
signTxServerIO env body =  do
  let skey = envSigningKey env

  let shelleyBasedEra = ShelleyBasedEraConway

      -- Creamos el witness usando la función correcta para eras Shelley-based
      witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentExtendedKey skey)

      -- Construimos la transacción firmada
  return $ makeSignedTransaction [witness] body





data UnsignedTx = UnsignedTx
  { cborHex :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)


newtype CBORData a= CBORData a deriving (Read, Show)


instance (Read a, Show a,SerialiseAsCBOR a) => Loggable (CBORData a) where
   serialize (CBORData c)   = byteString $  serialiseToCBOR c  `BS.snoc`   (0xFF :: Word8)
   deserialize = r where
     r = do
      either <-  deserialiseFromCBOR (asType :: AsType  a)  <$> BS.toStrict <$> tTakeWhile' (/= '\xFF')
      case either of
            Left err -> error $ "Error deserializando  " ++ show (typeOf (undefined :: a)) ++ "" ++show err
            Right x -> return $ CBORData x

-- Primitiva: firma en navegador (un solo paso) y devuelve la tx firmada completa


-- | firma con la clave privada envSingningKey 
signTxBrowser :: TxBodyContent BuildTx ConwayEra 
              -> Cloud (Tx ConwayEra)
signTxBrowser bodyContent = onAll $ do
  -- 1. Construimos y balanceamos, obtenemos el TxBody
  (cborHex, txBody) <-  do
    env <- ask
    liftIO $ do
      balanced@(BalancedTxBody _txContent txBody _change _fee)  <- buildAndBalanceTxIO env bodyContent
      let unsignedTx = makeSignedTransaction [] txBody
      let cborBytes = serialiseToCBOR unsignedTx   -- serializamos directamente el balancedTxBody
          cborHex = TE.decodeUtf8 $ B16.encode cborBytes
      return (cborHex, txBody)

  -- 2. Un solo paso: enviamos el unsigned y recibimos el witnessesHex limpio
  POSTData witnessesHex <- unCloud $ minput "sign" $ UnsignedTx { cborHex = cborHex }

  -- 3. Deserializamos el witness recibido del browser (el wallet ya lo firmó)
  do
    let witnessBytes = case B16.decode $ TE.encodeUtf8 witnessesHex of
          Left err   -> error $ "Hex inválido en witnesses: " ++ err
          Right bytes -> bytes

        witness :: KeyWitness ConwayEra
        witness =
          case deserialiseFromCBOR (asType :: AsType (KeyWitness ConwayEra)) witnessBytes of
            Left err -> error $ "Error deserializando witness: " ++ show err
            Right ws -> ws


  -- 4. Añadimos el witness del usuario al body (igual que en server-side)
    return $ makeSignedTransaction [witness] txBody






{-
submitSignedTx :: Tx ConwayEra -> Cloud TxId
submitSignedTx signedTx = do
  conn <- asks envConn
  result <- liftIO $ submitTxToNodeLocal conn (TxInMode signedTx ConwayEraInCardanoMode)
  case result of
    SubmitSuccess     -> return $ getTxId (txBody signedTx)
    SubmitFail reason -> error $ "Submit failed: " ++ show reason

buildAndSubmitTx :: TxBodyContent BuildTx ConwayEra -> Cloud TxId
buildAndSubmitTx content = do
  body   <- buildAndBalanceTx content
  signed <- signTx body
  submitSignedTx signed

-- 2. pay: Envía ADA a cualquier dirección
pay :: AddressInEra ConwayEra -> Lovelace -> Cloud TxId
pay targetAddr amount =
  buildAndSubmitTx $
    emptyTxBodyContent
      { txOuts =
          [ TxOut
              targetAddr
              (lovelaceToTxOutValue amount)
              TxOutDatumNone
              ReferenceScriptNone
          ]
      }

-- 1. lock: Bloquea fondos en un script con datum inline
lock :: AddressInEra ConwayEra -> Lovelace -> ScriptData -> Cloud TxId
lock scriptAddr amount datum =
  buildAndSubmitTx $
    emptyTxBodyContent
      { txOuts =
          [ TxOut
              scriptAddr
              (lovelaceToTxOutValue amount)
              (TxOutDatumInline era datum)
              ReferenceScriptNone
          ]
      }
  where
    era = BabbageEraOnwardsConway

-}