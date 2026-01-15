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
import System.IO
-- | An example computation that sends 5 ADA to a specific address.
--   It uses the `pay` primitive which encapsulates all the complexity.
paymentExample :: Cloud ()
paymentExample = do
    localIO $ putStrLn "Iniciando transacción de pago..."

    -- Destination address (replace with a real testnet address)
    let recipientAddrStr ="addr_test1vp9m89vegn6asp4ddenqpk7dvuw706p0fxuquqaz7g35n4qtyukzx"

    case deserialiseAddress (AsAddressInEra AsConwayEra) recipientAddrStr of
      Nothing -> localIO $ putStrLn "Error: Dirección de destino inválida."
      Just recipientAddr -> do
        -- Amount to send: 5 ADA
        let amountToSend = 1_000_000

        localIO $ putStrLn $ "Enviando " ++ show amountToSend ++ " lovelace a " ++ show recipientAddrStr

       
        txId <- pay recipientAddr amountToSend

        localIO $ putStrLn $ "¡Transacción enviada con éxito! TxId: " ++ show txId

main :: IO ()
main =  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin  utf8   
   
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
      local $ do
         ttr "before slot"
         sltno <- currentSlot
         ttr ("sltno",sltno)
      paymentExample
