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

module CardanoC.Api where

import Cardano.Api
import Cardano.Api.HasTypeProxy (AsType(..))
import Cardano.Api.Ledger(PParams)
import Cardano.Api.Query  -- (QueryEraHistory(..))




import Transient.Base
import Transient.Move
import Transient.Move.Web
import Transient.Move.Job
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
  eSKey <- readFileTextEnvelope  (File skeyPath)
  skey <- case eSKey of
    Left err  -> error $ "Error reading skey: " ++ show err
    Right k   -> return k

  -- Derive own address
  let vkey = castVerificationKey (getVerificationKey skey) :: VerificationKey PaymentKey
      pkh  = verificationKeyHash vkey
      ownAddr = makeShelleyAddressInEra ShelleyBasedEraConway networkId
                  (PaymentCredentialByKey pkh)
                  NoStakeAddress

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
  lifttIO $ waitUntilIO env targetSlot

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
  let conn = envConn env
      addrAny = case deserialiseAddress AsAddressAny (serialiseAddress addr) of
                  Nothing   -> error "Invalid address conversion"
                  Just any  -> any
      -- query :: QueryInMode CardanoMode (Either LedgerQueryFailure (UTxO ConwayEra))
      query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway -- EraInMode ConwayEra CardanoMode
                     (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny)))

  result <- liftIO $ runExceptT  $ queryNodeLocalState conn VolatileTip  query
  case result of
    Left acquireFail -> error $ "Acquire failed: " ++ show acquireFail
    Right (Left e)   -> error $ "Acquire failed: " ++ show e
    Right (Right utxo)       -> return utxo

-- Builds and balances, but does NOT sign or send

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

      -- Parameter for DRep deposit returns (empty if you don't unregister DReps)
      drepReturns :: Map.Map a Coin
      drepReturns = Map.empty

  let autoBalance = makeTransactionBodyAutoBalance
                      shelleyBasedEra
                      systemStart
                      epochInfo
                      ledgerPParams
                      stakePools
                      depositReturns
                      drepReturns  -- ← Here goes the "undefined" typed as Map.empty
                      utxo
                      bodyContent

      balancedEither = autoBalance ownAddr Nothing

  either (\err -> error $ "Error balancing tx: " ++ show err)
         return
         balancedEither


signTxServerIO :: CloudEnv -> TxBody ConwayEra -> IO (Tx ConwayEra)
signTxServerIO env body =  do
  let skey = envSigningKey env

  let shelleyBasedEra = ShelleyBasedEraConway

      -- We create the witness using the correct function for Shelley-based eras
      witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentExtendedKey skey)

      -- We build the signed transaction
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
            Left err -> error $ "Error deserializing  " ++ show (typeOf (undefined :: a)) ++ "" ++show err
            Right x -> return $ CBORData x

-- Primitive: sign in browser (one step) and return the complete signed tx


-- | signs with the private key envSingningKey
signTxBrowser :: TxBodyContent BuildTx ConwayEra 
              -> Cloud (Tx ConwayEra)
signTxBrowser bodyContent = onAll $ do
  -- 1. We build and balance, we get the TxBody
  (cborHex, txBody) <-  do
    env <- ask
    liftIO $ do
      balanced@(BalancedTxBody _txContent txBody _change _fee)  <- buildAndBalanceTxIO env bodyContent
      let unsignedTx = makeSignedTransaction [] txBody
      let cborBytes = serialiseToCBOR unsignedTx   -- we serialize the balancedTxBody directly
          cborHex = TE.decodeUtf8 $ B16.encode cborBytes
      return (cborHex, txBody)

  -- 2. One step: we send the unsigned and receive the clean witnessesHex
  POSTData witnessesHex <- unCloud $ minput "sign" $ UnsignedTx { cborHex = cborHex }

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

-- 2. pay: Sends ADA to any address
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

-- 1. lock: Locks funds in a script with inline datum
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