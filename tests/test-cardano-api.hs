import Cardano.Api
import Cardano.Api.Shelley

-- Construir una transacción simple
buildTx :: AddressInEra ShelleyAddr -> AddressInEra ShelleyAddr -> Lovelace -> IO (Tx ShelleyEra)
buildTx from to amount = do
    let txOut = TxOut to (lovelaceToValue amount) TxOutDatumNone ReferenceScriptNone
    -- Aquí construirías el cuerpo de la transacción y firmar
    pure $ makeShelleyTransactionFromTxOuts [txOut] -- simplificado
