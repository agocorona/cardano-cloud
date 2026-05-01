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

module CardanoC.Api  where

import Cardano.Api
import Cardano.Ledger.Coin(Coin)
import Cardano.Api.Ledger hiding(Tx,TxId,TxIn,Value,Testnet,Mainnet)

import CardanoC.MultiSignedFromBrowsers
import CardanoC.Sync
import Transient.Base
import Transient.Move
import Transient.Parse
import Transient.Move.Logged
import Transient.Move.Utils
import Transient.Move.Web
import Transient.Move.Job
-- import Transient.Indeterminism
import CardanoC.Defs

import Data.TCache

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Char(toLower)
import Data.Bifunctor (first)

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64

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
import Data.Either
import System.Random
import System.IO





import Control.Concurrent(threadDelay)
import Control.Exception hiding(onException)




-- -- 6. currentSlot → Directly uses getLocalChainTip with envConn
-- currentSlot :: TransIO (ChainTip,SlotNo)
-- currentSlot = do
--   env <- ask
--   tip <- liftIO $ getLocalChainTip (envConn env)
--   return $ case tip of
--     ChainTipAtGenesis -> (tip,SlotNo 0)
--     ChainTip slotNo _hash _blockNo -> (tip,slotNo)

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




-- data AcquireFailure = AcquireFailure String
-- -- | Gets all UTxOs from a list of addresses in a single query.
-- getUTxOsFromWalletAddressesIO :: MonadIO m => AppEnv -> [AddressInEra ConwayEra] -> m (SlotNo,UTxO ConwayEra)
-- getUTxOsFromWalletAddressesIO env addresses = liftIO $ do
--   let conn = envConn env
--       -- Correct form in modern cardano-api for AddressInEra -> AddressAny
--       toAddrAny (AddressInEra _ addr) = toAddressAny addr 

--       addressSet = Set.fromList $ map toAddrAny addresses

--   ttr ("getUTxOsFromWalletAddressesIO",length addresses, addresses)

--   let utxoFilter = QueryUTxOByAddress addressSet
--       query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway (QueryUTxO utxoFilter)

--   -- currentPoint <- do -- either (error . show) id <$> runExceptT $ queryNodeLocalState conn VolatileTip QueryChainPoint
--   --     result <-  runExceptT $ queryNodeLocalState conn VolatileTip QueryChainPoint
--   --     case result of
--   --        Right (Right c) -> return c
--   --        Left e -> do error e ; threadDelay 1000000;  getUTxOsFromWalletAddressesIO env addresses
--   --        e -> error e
--   currentPoint@(ChainPoint slotNo _) <- chainTipToChainPoint <$> getLocalChainTip (envConn env)
--   result <- runExceptT $ queryNodeLocalState conn (SpecificPoint currentPoint) query
--   ttr ("after getUTxOsFromWalletAddressesIO")
--   case result of
--     Left eb -> do print eb; getUTxOsFromWalletAddressesIO env addresses
--     Right (Left e) -> error $ show e
--     Right (Right utxo) -> do
--        valid <- isPointValid conn (SpecificPoint currentPoint)
--        if valid then return (slotNo,utxo) else do print "rollback happened"; threadDelay 1000000; getUTxOsFromWalletAddressesIO env addresses

--   where
--   -- isPointValid :: AppEnv -> ChainPoint -> IO Bool
--   isPointValid conn point = do

--     -- We try to make a minimal query (like requesting the current Slot) 
--     -- pointing specifically to that ChainPoint.
--     -- If the point has suffered a rollback, 'queryNodeLocalState' will fail 
--     -- before executing the internal query.
--     result <- runExceptT $ queryNodeLocalState conn point QueryChainPoint

--     case result of
--       -- If the node accepts the point, it is still in the main chain
--       Right _ -> return True

--       -- If there is a "PointNotFound" error or similar, the point is invalid
--       Left err -> do
--         putStrLn $ "Invalid point detected (possible rollback): " ++ show err
--         return False

--------------------------------------------------------------------------------------
-- 5. getUTxOsAt → Uses queryNodeLocalState with the existing connection
getUTxOsAtIO :: MonadIO m => AppEnv -> AddressInEra ConwayEra -> m (UTxO ConwayEra)
getUTxOsAtIO env addr = liftIO $ do
  ttr "getUTxOsAtIO"
  let conn = envConn env
      addrAny = case deserialiseAddress AsAddressAny (serialiseAddress addr) of
                  Nothing   -> error "Invalid address conversion"
                  Just any  -> any
      -- query :: QueryInMode CardanoMode (Either LedgerQueryFailure (UTxO ConwayEra))
      query = QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraConway -- EraInMode ConwayEra CardanoMode
                     (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny)))

  result <- liftIO $ runExceptT  $ queryNodeLocalState conn VolatileTip  query
  ttr "after getUTxOsAtIO"
  case result of
    Left acquireFail -> error $ "Acquire failed: " ++ show acquireFail
    Right (Left e)   -> error $ "Acquire failed: " ++ show e
    Right (Right utxo)       -> return utxo


newtype UnsignedTx = UnsignedTx
  { cborHex :: T.Text
  } deriving (Show, Generic, ToJSON, FromJSON)


newtype CBORData a= CBORData a deriving (Read, Show)


instance Loggable (KeyWitness ConwayEra)

-- instance Show (KeyWitness ConwayEra)
instance Read (KeyWitness ConwayEra) where
   readsPrec _ _= error "read not implemented"


deriving instance  Typeable (KeyWitness ConwayEra)

instance Loggable TxId where
  serialize txId= byteString $ serialiseToRawBytes  txId `BS.snoc` 0xFF
  deserialize= do
    mt <- deserialiseFromRawBytesHex . BS.toStrict <$> tTakeWhile' (/= '\xFF')
    case mt of
      Right tid -> return tid
      Left err ->  do
        s <- giveParseString
        error $ "Error deserializing  " ++ show (typeRep (Proxy :: Proxy TxId)) ++ " error: " <> show err










-- | signs with the private key envSingningKey
signSend :: Map.Map TxIn (TxOut CtxUTxO ConwayEra)
         -> BalancedTxBody ConwayEra
         -> Cloud TxId
signSend utxos balanced = local $ do
  env <- ask
  let localNodeInfo = envConn env
      network = localNodeNetworkId localNodeInfo
      addrs :: [AddressInEra ConwayEra] = nub . sort $ getAllRequiredSignerAddresses  network utxos balanced
      -- (myaddr  ,addrs ) = partition (== envOwnAddress env) alladdrs

  let (BalancedTxBody _txContent txBody _change _fee)  = balanced
      unsignedTx = makeSignedTransaction [] txBody
      cborBytes = serialiseToCBOR unsignedTx   -- we serialize the balancedTxBody directly
      cborHex = TE.decodeUtf8 $ B16.encode cborBytes

  -- let signserver = getWitnessServer env balanced

  ttr ("sign browser",length addrs)

  witnesss <-  unCloud $ collectc (length addrs) timeout $ do
    POSTData (witnessCbor :: T.Text) <- publishn (map (T.unpack . serialiseAddress) addrs) $  minput "signIt" $ UnsignedTx { cborHex = cborHex }
    ttr ("after sign",witnessCbor)

    moutput ("Thanks for signing" :: String)
    return witnessCbor

  
  let signed = case buildMultiSignedTx txBody  witnesss of
                    Left err     -> error $ show ("buildMultiSignedTx error", err)
                    Right signed -> signed


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





-- instance Loggable  TxId

retryTransaction :: TransIO ()
retryTransaction= forward (undefined :: TxValidationErrorInCardanoMode)


-- | Submits a signed transaction to the local Cardano node
submitSignedTx ::  Tx ConwayEra -> TransIO  TxId
submitSignedTx signedTx = do
  conn <- envConn <$> getState <|> error "error"  -- Assume this is LocalNodeConnectInfo CardanoMode

  -- Submit using the correct TxInMode wrapper for Conway
  ttr "submit"

  -- We wrap the \case in () to avoid GHC-77182 error
  onBack1 $ \e@(TxValidationErrorInCardanoMode _) -> do
      let errStr = map toLower $ show e
      if "mempoolfull" `isInfixOf` errStr then do
          loggedmsg "Mempool full - retrying"
          liftIO $ threadDelay 2000000
          retryTransaction
      else if "nodebusy" `isInfixOf` errStr then do
            loggedmsg "Node busy - retrying"
            liftIO $ threadDelay 1000000
            retryTransaction
      else backtrack

  result <- submitTxToNodeLocal  -- <|> rollbackhandling
              conn
              (TxInMode (shelleyBasedEra :: ShelleyBasedEra ConwayEra) signedTx)
  ttr "after submit"
  case result of
    SubmitSuccess -> do
        let txId = getTxId (getTxBody signedTx)
        liftIO $ trackTx txId
        return txId
    SubmitFail e@(TxValidationErrorInCardanoMode  _) ->  back e -- initiates the backtracking for transaction errors


-- | Autobalance a transaction
-- buildAndBalanceTxIO :: MonadIO m => AppEnv -> AddressInEra ConwayEra -> UTxO ConwayEra ->TxBodyContent BuildTx ConwayEra -> m (BalancedTxBody ConwayEra)

buildAndBalanceTxIO env ownAddr utxo bodyContent = liftIO $ do
  let conn       = envConn env
      -- ownAddr    = envOwnAddress env
      rawPParams = envPParams env


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
  -- ttr ("ownAddr:",envOwnAddress env)
  ttr ("complete bodyContent", bodyContent)  -- Check for hidden extras





  -- either (\err -> error $ "Error balancing tx: " ++ show err) 
  return balancedEither



timeout= 2*60*60*1000000

-- | pay ADA from the account defined in the application
payOne ::   AddressInEra ConwayEra -> Lovelace -> Cloud  TxId
payOne  targetAddr amount =  local $ do
  ttr "PAYONE"
  watchRawAddress targetAddr
  env <- getState <|> error "pay: no App state"
  userEnv <- getState <|> error "pay: no user env"
  let ownAddresses  = listPayAddresses $ userAddress userEnv
      stakeAddresses = listStakeAddresses $ userAddress userEnv
      changeAddr = fromMaybe (head ownAddresses) (changeAddress $ userAddress userEnv)

  payBactracks


  ttr ("Query all UTXOs at owner's address", length ownAddresses,ownAddresses)

  -- (slotNo,utxo) <- getUTxOsFromWalletAddressesIO env ownAddresses
  cachelist <- liftIO $ getUtxosFor  $ head stakeAddresses
  let utxo= toCardanoUtxo cachelist
      utxoList = Map.toList $ unUTxO utxo

  -- 3. The list of tuples (what you use for foldM, coin selectors, etc.)
  -- The type is: [(Api.TxIn, Api.TxOut Api.CtxUTxO Api.ConwayEra)]

  -- Check if there are any UTXOs
  when (null utxoList) $  output ("payIO: No UTXOs available at owner address" ::String) >> empty


  -- Convert all UTXOs to txIns with KeyWitnessForSpending
  let txInsList = map (\(utxoId, _) -> (utxoId, BuildTxWith (KeyWitness KeyWitnessForSpending))) utxoList

  -- Calculate TTL (Current Slot + 900 slots/15 minutes)
  tip <- liftIO $ getLocalChainTip (envConn env)
  let currentSlotNo = case tip of
        ChainTipAtGenesis -> SlotNo 0
        ChainTip s _ _ -> s
      ttlSlot = currentSlotNo + 900
      upperBound = TxValidityUpperBound ShelleyBasedEraConway (Just ttlSlot)

  -- the balanced, unsigned transaction body should be logged since a multisign can last for a long time
  -- makeTransactionBodyAutoSelection 
  ebody <- buildAndBalanceTxIO env changeAddr utxo $
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
                  , txValidityLowerBound = TxValidityNoLowerBound
                  , txValidityUpperBound = upperBound
                  }
  body <- case ebody of
    Left (TxBodyErrorMinUTxONotMet _ _) -> do output ("minimun amount not met, please retry" :: String) ; empty
    Right b -> return b
    err -> do output  $ show err ; empty

  unCloud job
  output ("Please sign the transaction" :: String)
  
  unCloud $ signSend  (toCtxTxOuts utxo) body
  where
  payBactracks= onBack1 $ \case
      e@(TxValidationErrorInCardanoMode _) -> do
          let errStr = map toLower $ show e

          -- 1. Double spend (BadInputsUTxO)
          if "badinputstuxo" `isInfixOf` errStr then do
              loggedmsg "Double spend detected - completely re-balancing"
              retryTransaction

          -- 2. Collateral (CollateralNotSufficient)
          else if "collateralnotsufficient" `isInfixOf` errStr then do
              loggedmsg "Insufficient collateral - adding collateral and re-balancing"
              retryTransaction

          -- 3. Insufficient funds (ValueNotConserved or InsufficientFunds)
          else if "valuenotconserved" `isInfixOf` errStr || "insufficientfunds" `isInfixOf` errStr then do
              loggedmsg "Insufficient funds - re-balancing with more inputs"
              retryTransaction

          -- 4. Fees (FeeTooSmall)
          else if "feetoosmall" `isInfixOf` errStr then do
              loggedmsg "Insufficient fee - recalculating fees"
              retryTransaction

          -- 5. Slot (ValidityInterval / TooEarly / Expired)
          else if "validityinterval" `isInfixOf` errStr || "TooEarly" `isInfixOf` errStr then do
              loggedmsg "Slot out of range - updating slot and re-balancing"
              retryTransaction

          else do
            loggedmsg $ "Unhandled error detected in JSON: " ++ errStr
            back e

      other -> back other


-- | Era-agnostic conversion from MiUTxO to Cardano API UTxO.
toCardanoUtxo :: forall era. (IsCardanoEra era) => [MiUTxO] -> UTxO era
toCardanoUtxo cacheList =
    UTxO $ Map.fromList [ (uId, convertToTxOut mi) | mi <- cacheList, let uId = utxoId mi ]
  where
    convertToTxOut :: MiUTxO -> TxOut CtxUTxO era
    convertToTxOut mi =
      let
        addrInEra = case anyAddressInEra (cardanoEra @era) (addr mi) of
          Right a -> a
          Left err -> error $ "Address conversion failed: " ++ err

        txOutVal = case cardanoEra @era of
          ByronEra   -> TxOutValueByron (selectLovelace (value mi))
          -- Shelley and Allegra: We use the Quantity conversion to get the numeric value
          ShelleyEra -> TxOutValueShelleyBased ShelleyBasedEraShelley (toShelleyLovelace (selectLovelace (value mi)))
          AllegraEra -> TxOutValueShelleyBased ShelleyBasedEraAllegra (toShelleyLovelace (selectLovelace (value mi)))
          -- Mary onwards
          MaryEra    -> TxOutValueShelleyBased ShelleyBasedEraMary    (toLedgerValue MaryEraOnwardsMary (value mi))
          AlonzoEra  -> TxOutValueShelleyBased ShelleyBasedEraAlonzo  (toLedgerValue MaryEraOnwardsAlonzo (value mi))
          BabbageEra -> TxOutValueShelleyBased ShelleyBasedEraBabbage (toLedgerValue MaryEraOnwardsBabbage (value mi))
          ConwayEra  -> TxOutValueShelleyBased ShelleyBasedEraConway  (toLedgerValue MaryEraOnwardsConway (value mi))
      in
      TxOut
        addrInEra
        txOutVal
        (convertDatum (datum mi) (datumHash mi))
        (convertRefScript (refScript mi))

    -- | Converts Lovelace to Coin using the Quantity representation.
    -- In v10.19, Lovelace is usually an alias of Quantity or has an instance of it.
    toShelleyLovelace :: Lovelace -> Cardano.Ledger.Coin.Coin
    toShelleyLovelace l =
      let Quantity q = lovelaceToQuantity l
      in fromIntegral q

    -- Witness helpers
    getBabbageWitness = case cardanoEra @era of
      BabbageEra -> Just BabbageEraOnwardsBabbage
      ConwayEra  -> Just BabbageEraOnwardsConway
      _          -> Nothing

    getAlonzoWitness = case cardanoEra @era of
      AlonzoEra  -> Just AlonzoEraOnwardsAlonzo
      BabbageEra -> Just AlonzoEraOnwardsBabbage
      ConwayEra  -> Just AlonzoEraOnwardsConway
      _          -> Nothing

    convertDatum mDat mHash = case (mDat, mHash) of
      (Just d, _) -> case getBabbageWitness of
                        Just w  -> TxOutDatumInline w d
                        Nothing -> TxOutDatumNone
      (_, Just h) -> case getAlonzoWitness of
                        Just w  -> TxOutDatumHash w h
                        Nothing -> TxOutDatumNone
      _           -> TxOutDatumNone

    convertRefScript (Just s) = case getBabbageWitness of
                                  Just w  -> ReferenceScript w s
                                  Nothing -> ReferenceScriptNone
    convertRefScript Nothing  = ReferenceScriptNone


-- 1. lock: Locks funds in a script with inline datum
-- lock :: AddressInEra ConwayEra -> Lovelace -> ScriptData -> Cloud  TxId
-- lock   scriptAddr amount datum = local $ do
--   env <- getState <|> error "error"
--   utxos <- liftIO $ getUTxOsAtIO env (envOwnAddress env)
--   body  <- liftIO $ buildAndBalanceTxIO  env (error "lock: fix this") utxos $
--           let 
--             shelleyBasedEra = ShelleyBasedEraConway
--             era = BabbageEraOnwardsConway
--             content= defaultTxBodyContent shelleyBasedEra 
--           in content
--             { txOuts =
--                 [ TxOut
--                       scriptAddr (lovelaceToTxOutValue shelleyBasedEra amount)
--                       (TxOutDatumInline era (unsafeHashableScriptData datum)) ReferenceScriptNone
--                 ]
--             -- CORRECTION: For the Conway era, it must be explicitly specified that there are no proposals.
--             , txProposalProcedures = Just (Featured ConwayEraOnwardsConway TxProposalProceduresNone)
--             }
--   unCloud $ signSend  (toCtxTxOuts utxos) body


toCtxTxOuts (UTxO utxoMap) = utxoMap


deriving instance Read NetworkMagic
deriving instance Read NetworkId
