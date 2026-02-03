-- Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

import Cardano.Api
import CardanoC.Api
import Transient.Base
import Transient.Move 
import Transient.Move.Utils
import Transient.Move.Web
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as BSS hiding(putStrLn)
import Data.Aeson
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
        -- let amountToSend = 1_000_000
        -- name <- minput "name" ("enter name" :: String) :: Cloud String
        -- surname <- minput "surname " ("enter surame" :: String) :: Cloud String
        -- minput "thanks"  (name <> " " <> surname)  :: Cloud ()
        -- empty
        POSTData amountToSend <- minput "pay"  ("pay option: choose amount" ::String) 
        moutput $ "Enviando " ++ show  amountToSend ++ " lovelace a " ++ show recipientAddrStr

        txId <- pay  recipientAddr  $ fromInteger amountToSend

        localIO $ putStrLn $ "¡Transacción enviada con éxito! TxId: " ++ show txId

-- main :: IO ()
-- main =  do
--     hSetEncoding stdout utf8
--     hSetEncoding stderr utf8
--     hSetEncoding stdin  utf8   
   
--     let socketPath = "/ipc/node.socket"
--         -- We use the Pre-production network as an example.
--         -- For other testnets, change the NetworkMagic. (e.g., Preview -> 2)
--         networkId = Testnet (NetworkMagic 2)
--         skeyPath = "./cardano-cloud/tests/payment.skey"
--     -- -------------------

--     putStrLn "Inicializando entorno de Cardano Cloud..."
--     -- 1. Initialize the environment only once.
--     env <- initAppEnv socketPath networkId skeyPath

--     putStrLn "Entorno inicializado. Ejecutando computación en la nube..."
--     -- 2. Execute the Cloud computation, which will be kept alive by `keep`.
--     --    `setState` makes the `env` available to all primitives.
--     void $ keep $ initNode $ do
--       setState env
--       local $ do
--          ttr "before slot"
--          sltno <- currentSlot
--          ttr ("sltno",sltno)
--       paymentExample

-- enterWallet= do
--   userInit
--   moutput ("OK" :: String)

-- mainaddex= do
--   ttr $ roundtripOk 

testHex =
  "01a4d541dc34e7f5ae9b3be9dfd490b6349ada0c917a40ca1ba8079cc35b0c18b423ad121ebadb0220f0850cad878dd0391abfadd3ae27b70f"

roundtripOk :: Bool
roundtripOk =
  let addr = fromHexToConwayAddress testHex 
  in toHexFromConwayAddress addr == testHex





    -- -- script hash (prefijo 0xe0 + 28 bytes hash)
    -- Just (0xe0, h) ->
    --   let sh = either (error . show) id (deserialiseFromRawBytes AsScriptHash h)
    --   in StakeCredentialByScript sh

    -- _ -> error "invalid stake credential"

-- maintrip= do
--    ttr test
--    ttr roundtripOk 


testdecode :: IO ()
testdecode = do
  let bs = BSS.pack jsonStr
  case eitherDecode bs of
    Left err -> System.IO.putStrLn ("Error decodificando: " <> err)
    Right ua -> print (ua :: UserAddress)
  
jsonStr :: String
jsonStr =
  "{\
  \  \"listPayAddresses\": [],\
  \  \"changeAddress\": \"01a4d541dc34e7f5ae9b3be9dfd490b6349ada0c917a40ca1ba8079cc35b0c18b423ad121ebadb0220f0850cad878dd0391abfadd3ae27b70f\",\
  \  \"listStakeAddresses\": [\
  \    \"e15b0c18b423ad121ebadb0220f0850cad878dd0391abfadd3ae27b70f\"\
  \  ]\
  \}"


test :: Bool
test =
  let h = BS.pack "e15b0c18b423ad121ebadb0220f0850cad878dd0391abfadd3ae27b70f"
  in toHexFromStakeCredential (fromHexToStakeCredential h) == h

main= runCC paymentExample
  -- "/ipc/node.socket"  
  -- (Testnet (NetworkMagic 2)) 
  -- "./cardano-cloud/tests/payment.skey" 

