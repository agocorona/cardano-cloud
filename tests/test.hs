-- Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

import Cardano.Api
import CardanoC.Api
import Transient.Base
import Transient.Move 
import Transient.Move.Utils
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Text

-- | An example computation that sends 5 ADA to a specific address.
--   It uses the `pay` primitive which encapsulates all the complexity.
paymentExample :: Cloud ()
paymentExample = local $ do
    liftIO $ putStrLn "Iniciando transacción de pago..."

    -- Destination address (replace with a real testnet address)
    let recipientAddrStr = "addr_test1..."

    case deserialiseAddress (AsAddressInEra AsConwayEra) recipientAddrStr of
      Nothing -> liftIO $ putStrLn "Error: Dirección de destino inválida."
      Just recipientAddr -> do
        -- Amount to send: 5 ADA
        let amountToSend = 5_000_000

        liftIO $ putStrLn $ "Enviando " ++ show amountToSend ++ " lovelace a " ++ show recipientAddrStr

        -- Your high-level primitive is used here!
        -- `pay` handles building, balancing, signing, and submitting the tx.
        -- NOTE: `pay` uses `signTxBrowser`, so it will wait for an interaction
        -- from a web client that responds to the "sign" endpoint.
        txId <- unCloud $ pay recipientAddr amountToSend

        liftIO $ putStrLn $ "¡Transacción enviada con éxito! TxId: " ++ show txId

-- | Main entry point of the application.
main :: IO ()
main =  do
    -- --- CONFIGURATION ---
    -- Replace these values with those of your testnet environment.
    let socketPath = "/ipc/node.socket"
        -- We use the Pre-production network as an example.
        -- For other testnets, change the NetworkMagic. (e.g., Preview -> 2)
        networkId = Testnet (NetworkMagic 2)
        skeyPath = "./cardano-cloud/tests/payment.skey"
    -- -------------------

    putStrLn "Inicializando entorno de Cardano Cloud..."
    -- 1. Initialize the environment only once.
    env <- initCloudEnv socketPath networkId skeyPath

    putStrLn "Entorno inicializado. Ejecutando computación en la nube..."
    -- 2. Execute the Cloud computation, which will be kept alive by `keep`.
    --    `setState` makes the `env` available to all primitives.
    void $ keep $ initNode $ do
       setState env
       localIO $ print "SUCCESS"
