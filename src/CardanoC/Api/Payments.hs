{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module CardanoC.Api.Payments where

import  Cardano.Api as Api

import qualified Data.Map.Strict as Map
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad
import           CardanoC.Defs
import           CardanoC.Api.UTxOSelector
import           Transient.Base

import qualified Data.Set as Set
import           Data.List

-- Requested type definitions
type AssetRequest = (Either Api.Lovelace Api.AssetId, Integer)
-- (Source, Assets, Destination, Change address for that source)
type Addr= Api.AddressInEra Api.ConwayEra

type PayRequest = (Addr, [AssetRequest], Addr, Addr)

retryTransaction :: TransIO ()
retryTransaction= forward (ofType :: TxValidationErrorInCardanoMode)

balanceMulti  :: AppEnv ->  [PayRequest] -> TransIO (Api.BalancedTxBody Api.ConwayEra)
balanceMulti env  requests = do
  -- increase margin buffer if some utxo is too small in the submission
  extra <- return (Coin 0) `onBack`  \case
      e@(TxValidationErrorInCardanoMode _) -> do
        let errStr = show e
        if "OutputTooSmallUTxO" `isInfixOf` errStr
            then do
                -- If it fails, we relaunch with a margin of 1.5 ADA (extra)
                -- This will make all 'processRequest' requests for more funds
                retryTransaction
                return $ Coin 1500000
            else back e
      other -> back other

  balanceMulti' env extra requests


-- | Main generalized function
balanceMulti' :: MonadIO m => AppEnv -> Lovelace -> [PayRequest] -> m (Api.BalancedTxBody Api.ConwayEra)
balanceMulti' env extraBuffer requests = liftIO $ do
  -- env <- ask
  when (null requests) $ error "No payment requests"

  -- 1. We use the first source address for the final fee adjustment
  let (firstSourceAddr, _, _, _) = head requests
  let safeMargin = if extraBuffer == 0 then 200000 else unCoin extraBuffer -- 0.2 ADA reserve per user

  -- 2. Process requests and accumulate TxIns, TxOuts and the total UTxO
  -- Note: Here you integrate your 'selectUtxos' function
  (allIns, allOuts, combinedUtxoMap) <- foldM (processRequest env safeMargin) ([], [], Map.empty) requests

  -- 3. Balance the transaction
  buildAndBalanceTxIO env (Api.UTxO combinedUtxoMap) firstSourceAddr $
    (Api.defaultTxBodyContent Api.ShelleyBasedEraConway)
      { Api.txIns = allIns
      , Api.txOuts = allOuts
      }

  -- -- THE FINAL SOLUTION:
  -- -- We convert the result of the balancing into a complete 'ghost' Tx
  -- -- to extract the ID, which is the only thing that matters to us.
  -- let tx = Api.makeSignedTransaction [] (Api.getBalancedTxBody balancedBody)

  -- return $ Api.getTxId (Api.getTxBody tx)

 where
  processRequest env feeReserve (accIns, accOuts, accUtxo) (srcAddr, assets, targetAddr, changeAddr) = do
    -- 1. Get UTXOs from the current address
    utxoRaw <- liftIO $ getUTxOsAtIO env srcAddr
    let utxoList = Map.toList (Api.unUTxO utxoRaw)

    -- 2. Selection of UTXOs including the fee reserve (which may include the extra from forward)
    case selectUtxos utxoList assets MinUtxos (Coin feeReserve) of
      Nothing -> error $ "Insufficient funds in " ++ show srcAddr
      Just selected -> do
        let selectedValue = foldMap (\(_, txOut) -> case txOut of { Api.TxOut _ val _ _ -> Api.txOutValueToValue val }) selected
        let paymentValue  = assetsToValue assets

        -- 3. Calculation of the potential change
        let changeValue = selectedValue
                          <> Api.negateValue paymentValue
                          <> Api.negateValue (Api.lovelaceToValue (fromInteger feeReserve))

        -- 4. Definition of the main payment
        let payOut = Api.TxOut targetAddr
                        (Api.TxOutValueShelleyBased Api.ShelleyBasedEraConway
                            (Api.toLedgerValue Api.MaryEraOnwardsConway paymentValue))
                        Api.TxOutDatumNone
                        Api.ReferenceScriptNone

        -- 5. DUST FILTER LOGIC (Dust Filter)
        -- We extract the Lovelace from the change to validate if it is enough
        let changeLovelace = Api.selectLovelace changeValue
        let minAdaThreshold = Api.Coin 1100000 -- 1.1 ADA as a security threshold

        let maybeChgOut =
              if changeLovelace < minAdaThreshold
              then [] -- If it is very little, we do not create the output. The ADA remains in the "well"
              else [Api.TxOut changeAddr
                        (Api.TxOutValueShelleyBased Api.ShelleyBasedEraConway
                            ((Api.toLedgerValue Api.MaryEraOnwardsConway) changeValue))
                        Api.TxOutDatumNone
                        Api.ReferenceScriptNone]

        -- 6. Mapping of new entries
        let newIns = map (\(tin, _) -> (tin, Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))) selected

        -- 7. We return accumulating. If maybeChgOut is [], simply nothing is added.
        return ( accIns ++ newIns
               , accOuts ++ [payOut] ++ maybeChgOut
               , accUtxo <> Map.fromList selected
               )

  assetsToValue = foldMap (\(e, amt) -> case e of
    Left _ -> Api.lovelaceToValue (fromInteger amt)
    Right aid -> Api.valueFromList [(aid, Api.Quantity amt)])

-- | Autobalance a transaction - Adapted version with dynamic change
buildAndBalanceTxIO
  :: MonadIO m
  => AppEnv
  -> UTxO ConwayEra
  -> AddressInEra ConwayEra  -- <--- We add this argument
  -> TxBodyContent BuildTx ConwayEra
  -> m (BalancedTxBody ConwayEra)
buildAndBalanceTxIO env utxo changeAddr bodyContent = liftIO $ do
  let conn       = envConn env
      rawPParams = envPParams env

  systemStart <- runExceptT (queryNodeLocalState conn VolatileTip QuerySystemStart)
                 >>= either (error . show) return

  -- If QueryEraHistory worked for you like this, we will NOT touch it
  eraHistory <- runExceptT (queryNodeLocalState conn VolatileTip QueryEraHistory)
                >>= either (error . show) return
  ttr "after era history"

  let shelleyBasedEra = ShelleyBasedEraConway
      epochInfo = toLedgerEpochInfo eraHistory
      ledgerPParams = LedgerProtocolParameters rawPParams
      stakePools = Set.empty
      depositReturns = Map.empty

      -- We keep your type correction for drepReturns
      -- drepReturns :: Map.Map (Credential 'DRepRole) Coin
      drepReturns = Map.empty

  ttr "autobalance with makeTransactionBodyAutoBalance"
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

      -- WE USE changeAddr instead of ownAddr
      balancedEither = do
        r <- autoBalance changeAddr Nothing
        ttr "after autobalance"
        return r

  ttr ("changeAddr used:", changeAddr)
  ttr ("complete bodyContent", bodyContent)

  either (\err -> error $ "Error balancing tx: " ++ show err) return balancedEither



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
  ttr "after getUTxOsAtIO"
  case result of
    Left acquireFail -> error $ "Acquire failed: " ++ show acquireFail
    Right (Left e)   -> error $ "Acquire failed: " ++ show e
    Right (Right utxo)       -> return utxo
