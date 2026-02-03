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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

{-|
Basic primitives for the creation of serverless, distributed, durable smart contracts
All the primitives in the Cloud monad share the properties of durability etc.
This module contains only the primitives that interface with Cardano Api
The goal is also to create an stable API, to free the DApp developer from the complexity 
and rapid changes of cardano Api.
TODO: identiry conditions of backtracking and implement them.
TODO: Testing

-}

module CardanoC.Api (module CardanoC.Api, module Transient.Base, module Transient.Move) where

import Cardano.Api

import Cardano.Api(TxOut(..))

-- import Cardano.Api.Shelley as Shelley  
import Cardano.Api.Ledger hiding(Tx,TxId,TxIn,Value,Testnet,Mainnet)   


-- import Ouroboros.Network.Protocol.LocalStateQuery.Type
  -- ( Target(..)          -- para GetTip, VolatileTip si existiera, etc.
  -- )


import Transient.Base
import Transient.Move
import Transient.Parse
import Transient.Move.Logged
import Transient.Move.Utils
import Transient.Move.Web
import Transient.Move.Job



import Control.Monad.IO.Class (liftIO)
import Control.Monad 
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Char(toLower)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- import qualified Data.Text.Lazy.Encoding as TE

import qualified Data.ByteString.Base16 as Base16



import Control.Concurrent(threadDelay)

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as BSS
import qualified Data.ByteString.Char8 as BSC

import Data.ByteString.Builder

import GHC.Generics

import Data.Aeson hiding (Value)
import Data.Aeson.Types (Parser)

import Data.Typeable
import Data.Word (Word8)
import Data.Default

import System.Random
import System.IO

data AppEnv = AppEnv
  { envConn       :: LocalNodeConnectInfo
  , envSigningKey :: SigningKey PaymentExtendedKey
  , envOwnAddress :: AddressInEra ConwayEra
  , envPParams    :: PParams (ShelleyLedgerEra ConwayEra)  -- Key change here
  , envNetworkId  :: NetworkId
  , envEra        :: AnyCardanoEra
  }

initAppEnv 
  :: FilePath
  -> NetworkId
  -> FilePath
  -> IO AppEnv
initAppEnv socketPath networkId skeyPath = do
  -- Load signing key
  eSKey <- readFileTextEnvelope (File skeyPath)
    :: IO (Either (FileError TextEnvelopeError) (SigningKey PaymentExtendedKey))
  skey <- case eSKey of
    Left err  -> error $ "Error reading skey: " ++ show err
    Right k   -> return k

  -- CORRECTION: Use the extended vkey to get the hash.
  -- We need to explicitly convert the VerificationKey PaymentExtendedKey
  -- to VerificationKey PaymentKey before getting the hash.
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

  putStrLn "AppEnv initialized correctly!"
  putStrLn $ "Network: " ++ show networkId
  putStrLn $ "Own address: " ++ show(serialiseAddress ownAddr)
  putStrLn $ "Socket: " ++ socketPath

  return AppEnv
    { envConn       = conn
    , envSigningKey = skey
    , envOwnAddress = ownAddr
    , envPParams    = pparams
    , envNetworkId  = networkId
    , envEra        = currentEra
    }




ask= getState <|> error ("cardano cloud: no state") :: TransIO AppEnv

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

waitUntilIO :: AppEnv -> SlotNo -> IO ()
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
getUTxOsAtIO :: AppEnv -> AddressInEra ConwayEra -> IO (UTxO ConwayEra)
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
  ttr("Available UTXOs: " , Map.size $ unUTxO utxo,"details", Map.toList $ unUTxO utxo)

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
buildAndBalanceTxAutoIO :: AppEnv -> TxBodyContent BuildTx ConwayEra -> IO (BalancedTxBody ConwayEra)
buildAndBalanceTxAutoIO env bodyContent = do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env  -- PParams (ShelleyLedgerEra ConwayEra)

  utxo <- getUTxOsAtIO env ownAddr
  ttr("Available UTXOs: " , Map.size $ unUTxO utxo,"details", Map.toList $ unUTxO utxo)
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




-- | Signs a balanced transaction (BalancedTxBody) using the environment's signing key.
--   It extracts the actual TxBody from the BalancedTxBody to be able to sign it.
signTxServerIO :: MonadIO m => AppEnv -> BalancedTxBody ConwayEra -> m (Tx ConwayEra)
signTxServerIO env balancedTxBody = do
  let skey = envSigningKey env  -- Assuming PaymentSigningKey or similar

  -- Extract the actual balanced TxBody (this is the one to be signed)
  let body = case balancedTxBody of
               BalancedTxBody _content txBody _change _fee -> txBody
               -- Or if your version uses different field names, adjust the pattern match

  -- Create the key witness using the function you already have and that compiles
  let shelleyEra = ShelleyBasedEraConway

      witness :: KeyWitness ConwayEra
      witness = makeShelleyKeyWitness shelleyEra body (WitnessPaymentExtendedKey skey)
        -- Or WitnessPaymentKey skey if it's not an extended key
  return $ makeSignedTransaction [witness] body
  -- Build the signed Tx

getWitnessServer :: AppEnv -> BalancedTxBody ConwayEra ->  KeyWitness ConwayEra
getWitnessServer env bbody= 
  let skey = envSigningKey env  -- Assuming PaymentSigningKey or similar

  -- Extract the real balanced TxBody (this is the one to be signed)
      body = case bbody of
               BalancedTxBody _content txBody _change _fee -> txBody
               -- Or if your version uses different field names, adjust the pattern match

  -- Create the key witness using the function you already have that compiles

      
  in makeShelleyKeyWitness ShelleyBasedEraConway body (WitnessPaymentExtendedKey skey)
        -- Or WitnessPaymentKey skey if it is not extended



data UnsignedTx = UnsignedTx
  { cborHex :: T.Text
  } deriving (Show, Generic, ToJSON, FromJSON)


newtype CBORData a= CBORData a deriving (Read, Show)


-- instance {-# OVERLAPPABLE #-}  (Read a, Show a, Typeable a,SerialiseAsCBOR a) => Loggable  a where
--    serialize  c   = byteString $  serialiseToCBOR c  `BS.snoc`   (0xFF :: Word8)
--    deserialize = r where
--      r = do
--       either <-  deserialiseFromCBOR (asType :: AsType  a)  <$> BS.toStrict <$> tTakeWhile' (/= '\xFF') 
--       case either of
--             Left err -> error $ "Error deserializing  " ++ show (typeOf (undefined :: a)) ++ "" ++show err
--             Right x -> return  x

instance Loggable (KeyWitness ConwayEra)

-- instance Show (KeyWitness ConwayEra)
instance Read ((KeyWitness ConwayEra)) where
   readsPrec _ _= error "read not implemented"


deriving instance  Typeable (KeyWitness ConwayEra)

instance Loggable TxId where
  serialize txId= byteString $ serialiseToRawBytes  txId `BS.snoc` 0xFF
  deserialize= do
    mt <- deserialiseFromRawBytesHex  <$> BS.toStrict <$> tTakeWhile' (/= '\xFF')
    case mt of
      Right tid -> return tid
      Left err ->  do
        s <- giveParseString
        error $ "Error deserializing  " ++ show (typeOf (undefined :: TxId)) ++ " error: " <> show err

-- | signs with the private key envSingningKey
signSend :: Map.Map TxIn (TxOut CtxUTxO ConwayEra)
         -> BalancedTxBody ConwayEra
         -> Cloud ( TxId)
signSend  utxos balanced = local $ do
  localNodeInfo <- envConn <$> getState <|> error "err"
  env <- getState <|> error "signSend: env state not found"
  let network = localNodeNetworkId localNodeInfo
      alladdrs :: [AddressInEra ConwayEra] = getAllRequiredSignerAddresses  network utxos balanced 
      (myaddr  ,addrs ) = partition (== envOwnAddress env) alladdrs
  
  let (BalancedTxBody _txContent txBody _change _fee)  = balanced 
      unsignedTx = makeSignedTransaction [] txBody
      cborBytes = serialiseToCBOR unsignedTx   -- we serialize the balancedTxBody directly
      cborHex = TE.decodeUtf8 $ B16.encode cborBytes

  let signserver = getWitnessServer env balanced

  ttr "sign browser"
  -- unCloud $ publishn (map (T.unpack . serialiseAddress) addrs) $ minput "sign" ("Please press here to receive and sign the transaction" :: String) :: TransIO ()

  witnesss <- unCloud $ collectc (length addrs) timeout $ do
    
    POSTData witnessesHex <- publishn (map (T.unpack . serialiseAddress) addrs) $ minput "signIt" $ UnsignedTx { cborHex = cborHex }
    ttr "after sign"

    -- 3. We deserialize the witness received from the browser (the wallet already signed it)
    let witnessBytes = case B16.decode $ TE.encodeUtf8 witnessesHex of
            Left err   -> error $ "Invalid hex in witnesses: " ++ err
            Right bytes -> bytes

          -- witness :: KeyWitness ConwayEra
          -- witness =
    return $ case deserialiseFromCBOR (asType :: AsType (KeyWitness ConwayEra)) witnessBytes of
              Left err -> error $ "Error deserializing witness: " ++ show err
              Right ws -> ws

  -- witnesss <- unCloud signAll

  -- 4. We add the user's witnesses to the body (same as on server-side)
  signed <- return $ makeSignedTransaction (signserver : witnesss) txBody
  submitSignedTx signed
  where 
  -- | Returns the addresses that must sign the transaction (payment key addresses only).
  --   Requires the UTxO map used to build the tx (to resolve TxIn -> AddressInEra). 
  -- | Returns the addresses that must sign the transaction (inputs with KeyWitness + extra required signers).
  getAllRequiredSignerAddresses
      :: NetworkId
      -> Map.Map TxIn (TxOut CtxUTxO ConwayEra)
      -> BalancedTxBody ConwayEra
      -> [AddressInEra ConwayEra]
  getAllRequiredSignerAddresses network utxoMap (BalancedTxBody bodyContent _change _collateral _fee) =
    nub (fromInputs ++ fromExtra)
    where
      fromInputs = mapMaybe getAddrFromInput (txIns bodyContent)

      getAddrFromInput :: (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn ConwayEra))
                      -> Maybe (AddressInEra ConwayEra) 
      getAddrFromInput (txin, BuildTxWith (KeyWitness _)) =   -- ← patrón correcto aquí
        case Map.lookup txin utxoMap of
          Just (TxOut addr _ _ _) -> Just addr
          _                       -> Nothing

      getAddrFromInput _ = Nothing

      -- Extra required signers (if you have them in your tx)
      fromExtra = mempty
        -- case txExtraKeyWits bodyContent of
        --   TxExtraKeyWitnesses _supported hashes ->
        --     [ let addrShelley = makeShelleyAddress network (PaymentCredentialByKey pkh) NoStakeAddress
        --       in case addrShelley of addr -> AddressInEra ConwayEra addr
        --     | pkh <- hashes
        --     ]
          -- _ -> []



-- instance Loggable  TxId

retryTransaction :: TransIO ()
retryTransaction= forward (undefined :: TxValidationErrorInCardanoMode)


-- | Submits a signed transaction to the local Cardano node
submitSignedTx ::  Tx ConwayEra -> TransIO  TxId
submitSignedTx signedTx = do
  conn <- envConn <$> getState <|> error "error"  -- Assume this is LocalNodeConnectInfo CardanoMode

  -- Submit using the correct TxInMode wrapper for Conway
  ttr "submit"

  -- onBack $ \case
  --   -- Mempool congestion (very frequent)
  --   SubmitTxErrorInMode _ (SubmitTxValidationError (MempoolFull _)) ->
  --     do loggedmsg "Mempool full - retrying submit"
  --        threadDelay 2000000  -- 2 seconds
  --        retryTransaction   -- → retry local (re-submit mismo signedTx)

  --   -- Other transient errors (e.g., node temporarily busy)
  --   SubmitTxErrorInMode _ (SubmitTxValidationError (NodeBusy _)) ->
  --     do loggedmsg "Node busy - retrying submit"
  --        threadDelay 1000000
  --        retryTransaction

  -- use queryUtxoByTxIn to verify utxos?

  result <- submitTxToNodeLocal
              conn 
              (TxInMode (shelleyBasedEra :: ShelleyBasedEra ConwayEra) signedTx)
  ttr "after submit"
  case result of
    SubmitSuccess -> return $ getTxId (getTxBody signedTx)
    SubmitFail e{- @(TxValidationErrorInCardanoMode  innerErr)-} -> back e 


-- | Autobalance a transaction
buildAndBalanceTxIO :: MonadIO m => AppEnv -> UTxO ConwayEra ->TxBodyContent BuildTx ConwayEra -> m (BalancedTxBody ConwayEra)
buildAndBalanceTxIO env utxo bodyContent = liftIO $ do
  let conn       = envConn env
      ownAddr    = envOwnAddress env
      rawPParams = envPParams env

  -- when the bug in balance is fixed:
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
      -- CORRECTION: We use the correct map types expected by the API, 
      -- specifying the concrete type for `drepReturns`.
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

      balancedEither = do r <- autoBalance ownAddr Nothing; ttr "after autobalance"; return r
  ttr ("ownAddr:",envOwnAddress env)
  ttr ("complete bodyContent", bodyContent)  -- Check for hidden extras
  ttr ("ownAddr serialized: ", case envOwnAddress env of
            AddressInEra _ addr -> serialiseAddress $ toAddressAny addr)



  either (\err -> error $ "Error balancing tx: " ++ show err) return balancedEither



timeout= 2*60*60*1000000

-- | pay ADA from the account defined in the application
pay ::   AddressInEra ConwayEra -> Lovelace -> Cloud  TxId
pay  targetAddr amount =  local $ do
  env <- getState <|> error "error"
  ownAddress <- head <$> listPayAddresses <$> userAddress <$> getState <|> error "error"

  -- rebalance in these submitTx errors. (To refine)
  -- onBack $ \case
  --   SubmitTxApplyTxErr (ApplyTxErr (UtxoFailure (BadInputsUTxO _))) ->
  --           retryTransaction   -- double spend → re-balanceo completo

  --   SubmitTxValidationError (CollateralNotSufficient _ _) ->
  --           retryTransaction   -- collateral → re-balanceo + añadir collateral

  --   SubmitTxValidationError (InsufficientFunds _ _) ->
  --           retryTransaction   -- fondos → re-balanceo con más inputs

  --   SubmitTxValidationError (FeeTooSmall _ _) ->
  --           retryTransaction   -- fee → re-balanceo (recalcula fees)

  --   SubmitTxValidationError (TxValidityTooEarly _) ->
  --           retryTransaction   -- slot → actualizar slot y re-balancear

  -- Query all UTXOs at owner's address
  output ("examining your utxos" :: String)
  utxo <- liftIO $ getUTxOsAtIO env ownAddress
  let utxoList = Map.toList (unUTxO utxo)
  
  -- Check if there are any UTXOs
  when (null utxoList) $ 
    error "payIO: No UTXOs available at owner address"
  
  -- Convert all UTXOs to txIns with KeyWitnessForSpending
  let txInsList = map (\(txIn, _) -> (txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))) utxoList
  
  -- the balanced, unsigned transaction body should be logged since a multisign can last for a long time
  body <- buildAndBalanceTxIO env utxo $
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
  unCloud $ signSend  (toCtxTxOuts utxo) body


-- 1. lock: Locks funds in a script with inline datum
lock :: AddressInEra ConwayEra -> Lovelace -> ScriptData -> Cloud  TxId
lock   scriptAddr amount datum = local $ do
  env <- getState <|> error "error"
  utxos <- liftIO $ getUTxOsAtIO env (envOwnAddress env)
  body  <- liftIO $ buildAndBalanceTxIO  env utxos $
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
            -- CORRECTION: For the Conway era, it must be explicitly specified that there are no proposals.
            , txProposalProcedures = Just (Featured ConwayEraOnwardsConway TxProposalProceduresNone)
            }
  unCloud $ signSend  (toCtxTxOuts utxos) body


toCtxTxOuts (UTxO utxoMap) = utxoMap

data UserAddress = UserAddress{listPayAddresses :: [AddressInEra ConwayEra],changeAddress :: Maybe(AddressInEra ConwayEra)
                              ,listStakeAddresses :: [StakeCredential]}  deriving (Generic,Default,Typeable)


  
-- 1) HEX -> AddressInEra ConwayEra
fromHexToConwayAddress
  :: BS.ByteString
  -> AddressInEra ConwayEra
fromHexToConwayAddress hex =
  case deserialiseFromRawBytesHex hex of
    Right addr ->  addr
    Left  left -> error $ show left


-- 2) AddressInEra ConwayEra -> HEX
toHexFromConwayAddress
  :: AddressInEra ConwayEra
  -> BS.ByteString
toHexFromConwayAddress =
  serialiseToRawBytesHex



fromHexToStakeCredential :: BS.ByteString -> StakeCredential
fromHexToStakeCredential =
  go . either (error "invalid hex") id . B16.decode
 where
  go bs = case BS.uncons bs of
    -- stake key hash
    Just (0xe1, h) ->
      let kh = either (error . show) id
                 (deserialiseFromRawBytes (AsHash AsStakeKey) h)
      in StakeCredentialByKey kh

    -- script hash (lo dejas como ya lo tenías)
    Just (0xe0, h) ->
      let sh = either (error . show) id (deserialiseFromRawBytes AsScriptHash h)
      in StakeCredentialByScript sh
 
    _ -> error "invalid stake credential"



    -- script hash (prefijo 0xe0 + 28 bytes hash)
    Just (0xe0, h) ->
      let sh = either (error . show) id (deserialiseFromRawBytes AsScriptHash h)
      in StakeCredentialByScript sh

    _ -> error "invalid stake credential" 


toHexFromStakeCredential :: StakeCredential -> BS.ByteString
toHexFromStakeCredential (StakeCredentialByKey kh) =
  B16.encode $ BS.cons 0xe1 (serialiseToRawBytes kh)
toHexFromStakeCredential (StakeCredentialByScript sh) =
  B16.encode $ BS.cons 0xe0 (serialiseToRawBytes sh)

instance ToJSON UserAddress where
  toJSON ua = object
    [ "listPayAddresses"   .= fmap (bsToText . toHexFromConwayAddress) (listPayAddresses ua)
    , "changeAddress"      .= fmap (bsToText . toHexFromConwayAddress) (changeAddress ua)
    , "listStakeAddresses" .= fmap (bsToText . toHexFromStakeCredential) (listStakeAddresses ua)
    ]
    where
      bsToText = TE.decodeUtf8

instance FromJSON UserAddress where
  parseJSON = withObject "UserAddress" $ \o ->
    UserAddress
      <$> fmap (map parseAddr) (o .:  "listPayAddresses")
      <*> fmap (fmap parseAddr) (o .:? "changeAddress")
      <*> fmap (map parseStake) (o .:  "listStakeAddresses")
    where
      parseAddr  = fromHexToConwayAddress . TE.encodeUtf8
      parseStake = fromHexToStakeCredential . TE.encodeUtf8



-- deriving instance Generic StakeCredential                          
-- -- deriving instance FromJSON StakeCredential

-- instance FromJSON StakeCredential where
--   parseJSON = withText "StakeCredential" $ \txt ->
--     let bsHex = TE.encodeUtf8 txt
--     in case B16.decode bsHex of
--          Right bs -> 
--            case deserialiseFromRawBytesHex bs of
--              Right h -> pure (StakeCredentialByKey h)
--              Left e  -> fail ("Invalid StakeCredential hash: " <> show e)
--          Left e -> fail ("StakeCredential hex mal formado: " <> e)

instance Default StakeCredential where 
  def = StakeCredentialByKey dummyHash
    where
      dummyHash =
        case B16.decode "1c3a7b0d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d" of
          Right bs ->
            case deserialiseFromRawBytes (AsHash AsStakeKey) bs of
              Right h  -> h
              Left err -> error $ show err
          Left e -> error ("malformed hex: " <> e)



instance Default (AddressInEra ConwayEra) where
  def =
    makeShelleyAddressInEra
      (shelleyBasedEra @ConwayEra) -- era
      Mainnet                      -- network
      (PaymentCredentialByKey dummyHash) -- dummy payment
      NoStakeAddress
    where
      dummyHash =
        case deserialiseFromRawBytesHex
               "0000000000000000000000000000000000000000000000000000000000000000" of
          Right h -> h
          Left e  -> error ("invalid dummy hash: " )




instance Loggable UserAddress where
  serialize= serializeToJSON
  deserialize=deserializeJSON

-- instance Default UserAddress where
--   def=  UserAddress{payAddresses=["$address1","$address2"] ,changeAddresses =["$changeaddres"] ,stakeAddresses=".."}
-- instance Generic UserAddress


data UserEnv = UserEnv{userAddress :: UserAddress }

-- | the account address is initiated and a endpoint to receive all the endpoint available
userInit :: Cloud ()
userInit = userInit' 

userInit'= do
  local abduce
  POSTData useraddr' <- minput "addrs" ("Enter your wallet addresses" :: String)
  useraddr <- if (null $ listPayAddresses useraddr') 
    then
      if changeAddress useraddr'== Nothing 
        then do
            moutput ("No payment addresses provided, please retry" :: String)
            empty 
        else
          return useraddr'{listPayAddresses= [fromJust $ changeAddress useraddr']}
    else return useraddr'

  setState UserEnv{userAddress= useraddr}
  onAllNodes $ do
    c <- liftIO $ BSS.pack <$> replicateM 5 (randomRIO ('a', 'z')) 
    setCookie "session" c  
    -- setSession $ BSS.toStrict c
  loggedmsg $ "Wallet registered with " ++ show (length $ listPayAddresses useraddr) ++ " payment addresses"
  -- publish the endpoint that returns all available endpoints at any moment
  --  note that <|> return () continues the execution for the initialization thread
  -- publishn ["allendpts"] (minput "allendpoints" ("see all the endpoints available for you" :: String) :: Cloud())
  allp <|> allPublishedEndpoints
  -- minput "next" ("next" ::String) :: Cloud ()
  

loggedmsg= local . return 

-- | send all the endpoints for a user. This may vary according with the user state and what other users publish
allp= do
 publish "General" $ minput "allendpts" ("All endpoints for you" :: String) :: Cloud()
 allPublishedEndpoints
 empty

allPublishedEndpoints = local $ do
    UserEnv{userAddress= UserAddress{listPayAddresses=addr:_}} <- getState

    output ( str "General") 
    ttr "after General" 
    published "general" 
    -- output (str "For you") 
    -- ttr "after For you"
    -- published (show addr)
    -- output (str "pending") 
    -- published ("pending" <> show addr)

str s= s :: String


-- main= runCC 
--   "/ipc/node.socket"  
--   (Testnet (NetworkMagic 2)) 
--   "./cardano-cloud/tests/payment.skey"  
--   empty
deriving instance Read NetworkMagic
deriving instance Read NetworkId
   
runCC :: Cloud () -> IO ()
runCC  cl=  void $ keep $ initNode $ inputNodes <|> do
    local $ do
      liftIO $ do
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8
        hSetEncoding stdin  utf8   
   
    -- let socketPath = "/ipc/node.socket"
    --     networkId = Testnet (NetworkMagic 2)
    --     skeyPath = "./cardano-cloud/tests/payment.skey"
    -- -------------------
      (socketPath, networkId, skeyPath) <- cardanoParams
      ttr (socketPath, networkId, skeyPath)
      liftIO $ putStrLn "Initalizing Cardano Cloud environment"
      -- 1. Initialize the environment only once.
      env <- liftIO $ initAppEnv socketPath networkId skeyPath
      setState env

    runJobs
    minput "init" ("init" :: String) :: Cloud ()

    userInit
    ttr "after userInit"
    cl
    where
    cardanoParams= do
      option "cardanoparams"  "Enter the Cardano params" :: TransIO String
      socketPath <- do
        input (const True) "Enter the socket path > "

      liftIO $ putStrLn $ "Choose the network:"
      networkId <- do
        (n :: String) <- input (const True) "Network id? >"
        case map Data.Char.toLower n of 
          "mainnet" -> return $ Mainnet
          "preprod" -> return $ Testnet(NetworkMagic 1)
          "preview" -> return $ Testnet(NetworkMagic 2)

      
      skeyPath <- input (const True) "Enter the path of your key file > " 
      
      return (socketPath, networkId, skeyPath)