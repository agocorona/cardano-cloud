{-# LANGUAGE OverloadedStrings #-}
import Network.WebSockets
import Data.Aeson (encode, decode, Value)

main :: IO ()
main = runClient "127.0.0.1" 1337 "/" clientApp

clientApp :: ClientApp ()
clientApp conn = do
    putStrLn "Conectado a Ogmios"
    
    -- Subscribe a la blockchain
    let subscribeMsg = encode $
            object ["type" .= ("Query" :: String), "query" .= object ["nextBlocks" .= Null]]
    sendTextData conn subscribeMsg
    
    -- Loop para recibir mensajes
    forever $ do
        msg <- receiveData conn
        putStrLn $ "Recibido: " ++ show (decode msg :: Maybe Value)
