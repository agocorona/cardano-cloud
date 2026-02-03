{-# LANGUAGE FlexibleContexts #-}
module CardanoC.Api.UTxOSelector (selectUtxos, SelectionStrategy() )where

import Cardano.Api
import Data.List (foldl', sortOn, find, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Coerce
type AssetRequest = (Either Lovelace AssetId, Integer)
type CUTxO = UTxO ConwayEra
data SelectionStrategy = MinUtxos | MinDust | ForTx | ForCollateral

getTxOut :: CUTxO -> Maybe (TxOut CtxUTxO ConwayEra)
getTxOut utxo = case Map.toList (unUTxO utxo) of
  [(txIn, txOut)] -> Just txOut
  _ -> Nothing

getLovelace :: CUTxO -> Integer
getLovelace utxo = 
  case getTxOut utxo of
    Just (TxOut _ txValue _ _) -> fromIntegral (unCoin (txOutValueToLovelace txValue))
    Nothing -> 0

getAssetQty :: AssetId -> CUTxO -> Integer
getAssetQty assetId utxo =
  case getTxOut utxo of
    Just (TxOut _ txValue _ _) -> 
      coerce (selectAsset (txOutValueToValue txValue) assetId :: Quantity)
    Nothing -> 0

isPureAda :: CUTxO -> Bool
isPureAda utxo =
  case getTxOut utxo of
    Just (TxOut _ txValue _ _) ->
      let value = txOutValueToValue txValue
          adaValue = lovelaceToValue (txOutValueToLovelace txValue)
      in value == adaValue
    Nothing -> False

addValues :: Value -> Value -> Value
addValues = mappend

isCollateralEligible :: CUTxO -> Bool
isCollateralEligible utxo = 
  case getTxOut utxo of
    Just (TxOut _ _ datum _) -> isPureAda utxo && datum == TxOutDatumNone
    Nothing -> False

selectUtxos :: [CUTxO] -> [AssetRequest] -> SelectionStrategy -> Maybe [CUTxO]
selectUtxos allUtxos requests strategy =
  let (adaReqs, tokenReqs) = partition isAdaRequest requests
      totalAdaNeeded = sum (map snd adaReqs)
      (selectedNFTs, afterNFTs) = selectNFTs allUtxos tokenReqs
      (selectedTokens, afterTokens) = selectFungibleTokens afterNFTs tokenReqs
      (selectedAda, remainingAda) = selectAda afterTokens totalAdaNeeded strategy
      allSelected = selectedNFTs ++ selectedTokens ++ selectedAda
  in if remainingAda <= 0 && allAssetsSatisfied allSelected requests
     then Just allSelected
     else Nothing

selectNFTs :: [CUTxO] -> [AssetRequest] -> ([CUTxO], [CUTxO])
selectNFTs utxos requests = foldl' collect ([], utxos) (filter (isNFTRequest utxos) requests)
  where
    collect (selected, remaining) (Right assetId, _) =
      case find (\u -> getAssetQty assetId u == 1) remaining of
        Just nftUtxo -> (nftUtxo:selected, filter (/= nftUtxo) remaining)
        Nothing -> (selected, remaining)
    collect acc _ = acc

selectFungibleTokens :: [CUTxO] -> [AssetRequest] -> ([CUTxO], [CUTxO])
selectFungibleTokens utxos requests = foldl' collect ([], utxos) fungibleReqs
  where
    fungibleReqs = filter (\(_, qty) -> qty > 1) requests
    collect (selected, remaining) (Right assetId, needed) =
      let candidates = filter (\u -> getAssetQty assetId u > 0) remaining
          sorted = sortOn (negate . getAssetQty assetId) candidates
          (picked, stillNeeded) = greedyPick sorted needed (Right assetId)
      in if stillNeeded <= 0
         then (selected ++ picked, filter (`notElem` picked) remaining)
         else (selected, remaining)
    collect acc _ = acc

selectAda :: [CUTxO] -> Integer -> SelectionStrategy -> ([CUTxO], Integer)
selectAda utxos needed strategy =
  let candidates = case strategy of
        MinUtxos -> sortOn (negate . getLovelace) utxos
        MinDust  -> sortOn getLovelace utxos
        ForTx    -> filter isPureAda $ sortOn (negate . getLovelace) utxos
        ForCollateral -> filter isCollateralEligible $ sortOn getLovelace utxos
      (selected, remaining) = greedyPick candidates needed (Left (Coin 0))
  in (selected, remaining)

greedyPick :: [CUTxO] -> Integer -> Either Lovelace AssetId -> ([CUTxO], Integer)
greedyPick [] needed _ = ([], needed)
greedyPick _ needed _ | needed <= 0 = ([], 0)
greedyPick (u:us) needed assetType =
  let amount = case assetType of
        Left _ -> getLovelace u
        Right assetId -> getAssetQty assetId u
      (rest, stillNeeded) = greedyPick us (needed - amount) assetType
  in (u:rest, stillNeeded)

allAssetsSatisfied :: [CUTxO] -> [AssetRequest] -> Bool
allAssetsSatisfied selected requests =
  let totalValue = foldl' addValues mempty (map getValue selected)
  in all (isRequestSatisfied totalValue) requests
  where
    getValue utxo = case getTxOut utxo of
      Just (TxOut _ txValue _ _) -> txOutValueToValue txValue
      Nothing -> mempty
    
    isRequestSatisfied totalValue (Left _, needed) =
      coerce (unCoin (selectLovelace totalValue)) >= needed
    
    isRequestSatisfied totalValue (Right assetId, needed) =
      coerce (selectAsset totalValue assetId :: Quantity) >= needed

isAdaRequest :: AssetRequest -> Bool
isAdaRequest (Left _, _) = True
isAdaRequest _ = False

isNFTRequest :: [CUTxO] -> AssetRequest -> Bool
isNFTRequest utxos (Right assetId, 1) = 
  any (\u -> getAssetQty assetId u == 1) utxos
isNFTRequest _ _ = False
