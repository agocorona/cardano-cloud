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
{-# LANGUAGE DataKinds #-}

{-|
Basic primitives for the creation of serverless, distributed, durable smart contracts
All the primitives in the Cloud monad share the properties of durability etc.
This module contains only the primitives that interface with Cardano Api
The goal is also to create an stable API, to free the DApp developer from the complexity 
and rapid changes of cardano Api.
TODO: separate the IO primitives from the Cloud primitives.
TODO: identiry conditions of backtracking and implement them.
TODO: Testing

-}

module CardanoC.Api (module CardanoC.Api, module Transient.Base, module Transient.Move) where

import Cardano.Api
-- import Cardano.Api.Shelley as Shelley  
import Cardano.Api.Ledger hiding(Tx,TxId,TxIn,Value)   
-- import Ouroboros.Network.Protocol.LocalStateQuery.Type
  -- ( Target(..)          -- para GetTip, VolatileTip si existiera, etc.
  -- )


import Transient.Base
import Transient.Move
import Transient.Move.Web
import Transient.Move.Job
import Transient.Parse


import Control.Monad.IO.Class (liftIO)
import Control.Monad 
import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as Base16



import Control.Concurrent(threadDelay)

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
  , envPParams    :: PParams (ShelleyLedgerEra ConwayEra)  -- Key change here
  , envNetworkId  :: NetworkId
  , envEra        :: AnyCardanoEra
  }

initCloudEnv 
  :: FilePath
  -> NetworkId
  -> FilePath
  -> IO CloudEnv
initCloudEnv socketPath networkId skeyPath = do
  -- Load signing key
  eSKey <- readFileTextEnvelope (File skeyPath)
    :: IO (Either (FileError TextEnvelopeError) (SigningKey PaymentExtendedKey))
  skey <- case eSKey of
    Left err  -> error $ "Error reading skey: " ++ show err
    Right k   -> return k

  -- CORRECCIÓN: Usar la vkey extendida para obtener el hash.
  -- Necesitamos convertir explícitamente la VerificationKey PaymentExtendedKey
  -- a VerificationKey PaymentKey antes de obtener el hash.
  let vkeyExtended = getVerificationKey skey
      vkeyPayment  = castVerificationKey vkeyExtended :: VerificationKey PaymentKey
      pkh          = verificationKeyHash vkeyPayment
      ownAddr = makeShelleyAddressInEra ShelleyBasedEraConway networkId
                  (PaymentCredentialByKey pkh)
                  NoStakeAddress
                  
  putStrLn $ "Own address: " ++ show(serialiseAddress ownAddr)

  -- Local connection
  let conn = LocalNodeConnectInfo
               { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
               , localNodeNetworkId       = networkId
               , localNodeSocketPath      = File socketPath
               }

  -- Query PParams (current way in Conway)
  eResult <- runExceptT $ queryNodeLocalState conn VolatileTip
                        (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters))

  pparams <- case eResult of
               Left acquiringErr -> error $ "Failed to acquire connection to the node: " ++ show acquiringErr
               Right (Left eraMismatch) -> error $ "Era mismatch in PParams query: " ++ show eraMismatch
               Right (Right pp) -> return pp  -- pp :: PParams ConwayEra (from ledger, matches your envPParams)
  let currentEra = AnyCardanoEra ConwayEra

  putStrLn "CloudEnv initialized correctly!"
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
-- Key primitives using the reused connection
-- ===========================================================================

-- 6. currentSlot → Directly uses getLocalChainTip with envConn
currentSlot :: TransIO SlotNo
currentSlot = do
  env <- ask
  tip <- liftIO $ getLocalChainTip (envConn env)
  return $ case tip of
    ChainTipAtGenesis -> SlotNo 0
    ChainTip slotNo _hash _blockNo -> slotNo

-- 4. waitUntil → Efficient polling using the same connection
waitUntil :: SlotNo -> Cloud ()
waitUntil targetSlot = local $ do
  env <- ask
  liftIO $ waitUntilIO env targetSlot

waitUntilIO :: CloudEnv -> SlotNo -> IO ()
waitUntilIO env targetSlot= do
  let conn = envConn env
      loop = do
        tip <- getLocalChainTip conn
        let current = case tip of
              ChainTipAtGenesis -> SlotNo 0
              ChainTip s _ _ -> s
        if current >= targetSlot
          then return ()
          else do
            liftIO $ threadDelay 1_000_000 -- 1 second (adjustable)
            loop
  void loop

-- | Wait for a duration in seconds
waitSeconds :: Int -> Cloud ()
waitSeconds secs =  do
  tinit <- local $ fromIntegral <$> getMicroSeconds
  tnow  <- onAll $ fromIntegral <$> getMicroSeconds
  let tfin= tinit + secs * 1_000_000

  let tleft= tfin - tnow
  onAll $ liftIO $ when (tleft > 0) $ threadDelay tleft


-- 5. getUTxOsAt → Uses queryNodeLocalState with the existing connection
getUTxOsAtIO :: CloudEnv -> AddressInEra ConwayEra -> IO (UTxO ConwayEra)
getUTxOsAtIO env addr = do
  ttr "getUTxOsAtIO"
  let conn = envConn env
      addrAny = case deserialiseAddress AsAddressAny (serialiseAddress addr) of
                  Nothing   -> error "Invalid address conversion"
                  Just any  -> any
      -- query :: QueryInMode CardanoMode (Either LedgerQueryFailure (UTxO ConwayEra))
      query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway -- EraInMode ConwayEra CardanoMode
                     (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny)))

  result <- liftIO $ runExceptT  $ queryNodeLocalState conn VolatileTip  query
  ttr ("after getUTxOsAtIO")
  case result of
    Left acquireFail -> error $ "Acquire failed: " ++ show acquireFail
    Right (Left e)   -> error $ "Acquire failed: " ++ show e
    Right (Right utxo)       -> return utxo


buildAndBalanceTxEstimateIO env bodyContent = do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env  -- PParams (ShelleyLedgerEra ConwayEra)
  
  utxo <- getUTxOsAtIO env ownAddr
  ttr("UTXOs disponibles: " , Map.size $ unUTxO utxo,"details", Map.toList $ unUTxO utxo)

  let shelleyBasedEra = ShelleyBasedEraConway
      maryEraOnwards = MaryEraOnwardsConway

      stakePools :: Set.Set PoolId
      stakePools = Set.empty

      depositReturns :: Map.Map StakeCredential Coin
      depositReturns = Map.empty

      -- drepReturns :: Map.Map DRepCredential Coin
      drepReturns = Map.empty

      exUnitsMap :: Map.Map ScriptWitnessIndex ExecutionUnits
      exUnitsMap = Map.empty

      initialFee :: Coin
      initialFee = Coin 0

      keyWits :: Int
      keyWits = 1

      scriptWits :: Int
      scriptWits = 0

      refSize :: Int
      refSize = 0

      selectedTxIns = Map.keys (unUTxO utxo)

      totalInputValue :: Value
      totalInputValue = foldMap go (Map.elems $ unUTxO utxo)
        where
          go (TxOut _addr txOutVal _datum _refScript) = txOutValueToValue txOutVal



      bodyContent' = bodyContent { txIns = txIns bodyContent ++ map (\txin -> (txin, BuildTxWith (KeyWitness KeyWitnessForSpending))) selectedTxIns }

  ttr "autobalance"
  let balancedEither = estimateBalancedTxBody
                      maryEraOnwards
                      bodyContent'
                      rawPParams
                      stakePools
                      depositReturns
                      drepReturns
                      exUnitsMap
                      initialFee
                      keyWits
                      scriptWits
                      refSize
                      ownAddr
                      totalInputValue

  either (\err -> error $ "Error balancing tx: " ++ show err)
         return
         (do r <-balancedEither; ttr "after autobalance"; return r)

-- | balancing using makeTransactionBodyAutoBalance
buildAndBalanceTxAutoIO :: CloudEnv -> TxBodyContent BuildTx ConwayEra -> IO (BalancedTxBody ConwayEra)
buildAndBalanceTxAutoIO env bodyContent = do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env  -- PParams (ShelleyLedgerEra ConwayEra)

  utxo <- getUTxOsAtIO env ownAddr
  ttr("UTXOs disponibles: " , Map.size $ unUTxO utxo,"details", Map.toList $ unUTxO utxo)
  systemStart <- runExceptT (queryNodeLocalState conn VolatileTip QuerySystemStart)
                 >>= either (error . show) return

  eraHistory <- runExceptT (queryNodeLocalState conn VolatileTip QueryEraHistory)
                >>= either (error . show) return
  ttr "after era history"
  let shelleyBasedEra = ShelleyBasedEraConway

      epochInfo = toLedgerEpochInfo eraHistory

      ledgerPParams = LedgerProtocolParameters rawPParams

      stakePools :: Set.Set PoolId
      stakePools = Set.empty

      depositReturns :: Map.Map StakeCredential Coin
      depositReturns = Map.empty

      -- Parameter for DRep deposit returns (empty if you don't unregister DReps)
      drepReturns :: Map.Map (Credential 'DRepRole) Coin
      drepReturns = Map.empty
  ttr "autobalance"
  let autoBalance = makeTransactionBodyAutoBalance
                      shelleyBasedEra
                      systemStart
                      epochInfo
                      ledgerPParams
                      stakePools
                      depositReturns
                      drepReturns 
                      utxo
                      bodyContent
      
      balancedEither = do r <- autoBalance ownAddr Nothing;   ttr ("after autobalance"); return r
  
   
  either (\err -> error $ "Error balancing tx: " ++ show err)
         return
         balancedEither



-- | Firma una transacción balanceada (BalancedTxBody) usando la signing key del entorno.
--   Extrae el TxBody real del BalancedTxBody para poder firmarlo.
signTxServerIO :: MonadIO m => CloudEnv -> BalancedTxBody ConwayEra -> m ( Tx ConwayEra)
signTxServerIO env balancedTxBody = do
  let skey = envSigningKey env  -- Asumimos PaymentSigningKey o similar

  -- Extraer el TxBody real ya balanceado (este es el que se firma)
  let body = case balancedTxBody of
               BalancedTxBody _content txBody _change _fee -> txBody
               -- O si tu versión usa nombres de campos diferentes, ajusta el pattern match

  -- Crear el witness de clave usando la función que ya tienes y compila
  let shelleyEra = ShelleyBasedEraConway

      witness :: KeyWitness ConwayEra
      witness = makeShelleyKeyWitness shelleyEra body (WitnessPaymentExtendedKey skey)
        -- O WitnessPaymentKey skey si no es extended key

  -- Construir la Tx firmada
  pure $ makeSignedTransaction [witness] body





data UnsignedTx = UnsignedTx
  { cborHex :: T.Text
  } deriving (Show, Generic, ToJSON, FromJSON)


newtype CBORData a= CBORData a deriving (Read, Show)


instance (Read a, Show a,SerialiseAsCBOR a) => Loggable (CBORData a) where
   serialize (CBORData c)   = byteString $  serialiseToCBOR c  `BS.snoc`   (0xFF :: Word8)
   deserialize = r where
     r = do
      either <-  deserialiseFromCBOR (asType :: AsType  a)  <$> BS.toStrict <$> tTakeWhile' (/= '\xFF')
      case either of
            Left err -> error $ "Error deserializing  " ++ show (typeOf (undefined :: a)) ++ "" ++show err
            Right x -> return $ CBORData x

-- Primitive: sign in browser (one step) and return the complete signed tx


-- | signs with the private key envSingningKey
signTxBrowser ::  BalancedTxBody ConwayEra
              -> TransIO ( Tx ConwayEra)
signTxBrowser balanced = do
  -- 1. We build and balance, we get the TxBody
  (cborHex, txBody) <-  do
    env <- ask
    liftIO $ do
      let (BalancedTxBody _txContent txBody _change _fee)  = balanced 
          unsignedTx = makeSignedTransaction [] txBody
          cborBytes = serialiseToCBOR unsignedTx   -- we serialize the balancedTxBody directly
          cborHex = TE.decodeUtf8 $ B16.encode cborBytes
      return (cborHex, txBody)
  ttr "sign browser"
  -- 2. One step: we send the unsigned and receive the clean witnessesHex
  POSTData witnessesHex <- unCloud $ minput "sign" $ UnsignedTx { cborHex = cborHex }
  ttr "after sign"

  -- 3. We deserialize the witness received from the browser (the wallet already signed it)
  do
    let witnessBytes = case B16.decode $ TE.encodeUtf8 witnessesHex of
          Left err   -> error $ "Invalid hex in witnesses: " ++ err
          Right bytes -> bytes

        witness :: KeyWitness ConwayEra
        witness =
          case deserialiseFromCBOR (asType :: AsType (KeyWitness ConwayEra)) witnessBytes of
            Left err -> error $ "Error deserializing witness: " ++ show err
            Right ws -> ws


  -- 4. We add the user's witness to the body (same as on server-side)
    return $ makeSignedTransaction [witness] txBody






instance Loggable  TxId


submitSignedTx ::  Tx ConwayEra -> Cloud  TxId
submitSignedTx signedTx = local $ do
  env <-  ask 
  liftIO $ submitSignedTxIO env signedTx

-- | Submits a signed transaction to the local Cardano node
--   and returns the transaction ID upon success.
submitSignedTxIO :: CloudEnv ->  Tx ConwayEra -> IO  TxId
submitSignedTxIO env signedTx = do
  let conn = envConn env  -- Assume this is LocalNodeConnectInfo CardanoMode

  -- Submit using the correct TxInMode wrapper for Conway
  ttr "submit"
  result <- submitTxToNodeLocal
              conn
              (TxInMode (shelleyBasedEra :: ShelleyBasedEra ConwayEra) signedTx)
  ttr "after submit"
  case result of
    SubmitSuccess -> pure $ getTxId (getTxBody signedTx)
    SubmitFail reason -> error $ "Submit failed: " ++ show reason

-- | Nueva función de balanceo que utiliza `makeTransactionBodyAutoBalance`
--   corrigiendo el problema de ambigüedad de tipos que causaba el bloqueo.
buildAndBalanceTxIO :: CloudEnv -> UTxO ConwayEra ->TxBodyContent BuildTx ConwayEra -> IO (BalancedTxBody ConwayEra)
buildAndBalanceTxIO env utxo bodyContent = do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env

  -- utxo <- getUTxOsAtIO env ownAddr
  -- ttr("UTXOs disponibles: " , Map.size $ unUTxO utxo,"details", Map.toList $ unUTxO utxo)
  systemStart <- runExceptT (queryNodeLocalState conn VolatileTip QuerySystemStart)
                 >>= either (error . show) return

  eraHistory <- runExceptT (queryNodeLocalState conn VolatileTip QueryEraHistory)
                >>= either (error . show) return
  ttr "after era history"

  let shelleyBasedEra = ShelleyBasedEraConway
      epochInfo = toLedgerEpochInfo eraHistory
      ledgerPParams = LedgerProtocolParameters rawPParams
      stakePools = Set.empty
      depositReturns = Map.empty
      -- CORRECCIÓN: Usamos los tipos de mapa correctos que espera la API,
      -- especificando el tipo concreto para `drepReturns`.
      drepReturns :: Map.Map (Credential 'DRepRole) Coin
      drepReturns = Map.empty

  ttr "autobalance con makeTransactionBodyAutoBalance"
  let autoBalance = makeTransactionBodyAutoBalance
                      shelleyBasedEra
                      systemStart
                      epochInfo
                      ledgerPParams
                      stakePools
                      depositReturns
                      drepReturns
                      
                      utxo
                      bodyContent

      balancedEither = do r <- autoBalance ownAddr Nothing; ttr ("after autobalance"); return r
  ttr ("ownAddr:",envOwnAddress env)
  ttr ("bodyContent completo", bodyContent)  -- Ver si hay extras ocultos
  ttr ("ownAddr serialized: ", case envOwnAddress env of
            AddressInEra _ addr -> serialiseAddress $ toAddressAny addr)



  either (\err -> error $ "Error balancing tx: " ++ show err) return balancedEither

-- | Nueva función de construcción y envío que usa el autobalanceo corregido.
buildAndSubmitTx ::  TxBodyContent BuildTx ConwayEra -> Cloud  TxId
buildAndSubmitTx  content = local $ do
  env <- ask
  utxo <-  liftIO $ getUTxOsAtIO env (envOwnAddress env)
  buildAndSubmitTxIO env utxo content

buildAndSubmitTxIO :: MonadIO m => CloudEnv -> UTxO ConwayEra -> TxBodyContent BuildTx ConwayEra -> m  TxId
buildAndSubmitTxIO env utxo content= do
  ttr "balance"
  body   <-  liftIO $ buildAndBalanceTxIO env utxo content -- Llama a la nueva función de balanceo
  ttr "after balance"
  signed <- signTxServerIO env body
  liftIO $ submitSignedTxIO env signed





pay ::   AddressInEra ConwayEra -> Lovelace -> Cloud  TxId
pay  targetAddr amount = local $ do
  env <- getState <|> error "pay: No env"
  payIO env targetAddr amount 


-- 2. pay: Sends ADA to any address
payIO :: MonadIO m => CloudEnv -> AddressInEra ConwayEra -> Lovelace -> m TxId
payIO env targetAddr amount = liftIO $ do
  -- Query all UTXOs at owner's address
  -- This is a temporal fix, since the balance in general in 10.16- 10.19 versions of cardano-api seems that it don't work with empty txIns
  utxo <- getUTxOsAtIO env (envOwnAddress env)
  let utxoList = Map.toList (unUTxO utxo)
  
  -- Check if there are any UTXOs
  when (null utxoList) $ 
    error "payIO: No UTXOs available at owner address"
  
  -- Convert all UTXOs to txIns with KeyWitnessForSpending
  let txInsList = map (\(txIn, _) -> (txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))) utxoList
  
  buildAndSubmitTxIO env utxo $
    let shelleyBasedEra = ShelleyBasedEraConway
        bodyContent = defaultTxBodyContent shelleyBasedEra
    in bodyContent
      { txIns = txInsList  -- Use all UTXOs as inputs
      , txOuts =
          [ TxOut
              targetAddr
              (lovelaceToTxOutValue shelleyBasedEra amount)
              TxOutDatumNone
              ReferenceScriptNone
          ]
      , txProposalProcedures = Nothing
      , txVotingProcedures = Nothing
      , txCertificates = TxCertificatesNone
      , txWithdrawals = TxWithdrawalsNone
      }


-- 1. lock: Locks funds in a script with inline datum
lock :: AddressInEra ConwayEra -> Lovelace -> ScriptData -> Cloud  TxId
lock scriptAddr amount datum =
  buildAndSubmitTx $ -- Usamos el nuevo `buildAndSubmitTx`
    let 
      shelleyBasedEra = ShelleyBasedEraConway
      era = BabbageEraOnwardsConway
      content= defaultTxBodyContent shelleyBasedEra 
    in content
      { txOuts =
          [ TxOut
                scriptAddr (lovelaceToTxOutValue shelleyBasedEra amount)
                (TxOutDatumInline era (unsafeHashableScriptData datum)) ReferenceScriptNone
          ]
      -- CORRECCIÓN: Para la era Conway, se debe especificar explícitamente que no hay propuestas.
      , txProposalProcedures = Just (Featured ConwayEraOnwardsConway TxProposalProceduresNone)
      }

