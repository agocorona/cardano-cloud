{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-} -- Añadimos esto para el error de "WitnessSet"
{-# LANGUAGE FlexibleContexts #-} -- Añadimos esto para el error de "WitnessSet"

module CardanoC.MultiSignedFromBrowsers where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS -- Para el error de Lazy vs Strict
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Bifunctor (first)
import qualified Data.Set as Set

import qualified Cardano.Api as Api
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Ledger.Binary as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger

-- | Construye la transacción combinando el cuerpo y los testigos de CIP-30
buildMultiSignedTx
  :: Api.TxBody Api.ConwayEra
  -> [T.Text]     -- hex(cbor<transaction_witness_set>)
  -> Either String (Api.Tx Api.ConwayEra)
buildMultiSignedTx txBody witHexes = do
  -- 1. Decodificamos forzando la era del Ledger de Conway
  (ledgerWitsList :: [Ledger.AlonzoTxWits (Api.ShelleyLedgerEra Api.ConwayEra)]) 
    <- mapM decodeWitnessSet witHexes

  -- 2. Extraer los VKey witnesses mediante pattern matching directo
  let allKeyWits = 
        [ Api.ShelleyKeyWitness Api.ShelleyBasedEraConway vk
        | Ledger.AlonzoTxWits vkSet _ _ _ _ <- ledgerWitsList
        , vk <- Set.toList vkSet
        ]

  -- 3. Ensamblar la transacción final
  pure $ Api.makeSignedTransaction allKeyWits txBody

 where
  decodeWitnessSet t = do
    bs <- first (const "hex inválido") $ B16.decode (TE.encodeUtf8 t)
    let protVer = Api.eraProtVerLow Api.ShelleyBasedEraConway
    
    -- "WitnessSet" ahora será T.Text gracias a OverloadedStrings
    -- LBS.fromStrict bs convierte el ByteString al formato Lazy que pide Ledger
    first show $ Ledger.decodeFullAnnotator 
                   protVer 
                   (T.pack "WitnessSet") -- O simplemente "WitnessSet" con OverloadedStrings
                   Ledger.decCBOR 
                   (LBS.fromStrict bs)