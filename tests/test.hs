{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Cardano.Api
-- import Cardano.Api.Ledger (TxWits (..))
-- import Cardano.Binary (decCBOR)

import CardanoC.Defs
import CardanoC.Api
import CardanoC.SF
import CardanoC.Sync
import Transient.Base
import Transient.Console
import Transient.Indeterminism
import Transient.Move
import Transient.Move.Logged
import Transient.Move.Defs
import Transient.Move.Utils
import Transient.Move.Job
import Transient.Move.Web
import Data.TCache
import Data.TCache.IndexQuery (index)
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BSS hiding(putStrLn)
import Data.Aeson
import qualified Data.Map as Map
import System.IO
import qualified GHC.Stats  -- DEBUG: post-mortem RTS monitor (remove when done)
import Data.Time (getCurrentTime)  -- DEBUG: post-mortem RTS monitor (remove when done)
import Data.Typeable
import Data.Maybe
import qualified Data.Set as Set
import Data.IORef
import Control.Monad.State
import GHC.Generics
import Data.Default

-- | An example computation that sends 5 ADA to a specific address.
--   It uses the `pay` primitive which encapsulates all the complexity.
paymentExample :: Cloud ()
paymentExample = do
    publish "General" $ minput "payment_example"  ("pay Example" :: String) :: Cloud()
    localIO $ putStrLn "Iniciando transacción de pago..."

    -- Destination address (replace with a real testnet address)
    let recipientAddrStr ="addr_test1vp9m89vegn6asp4ddenqpk7dvuw706p0fxuquqaz7g35n4qtyukzx"

    case deserialiseAddress (AsAddressInEra AsConwayEra) recipientAddrStr of
      Nothing -> localIO $ putStrLn "Error: Dirección de destino inválida."
      Just recipientAddr -> do

        addrs <- local $ do
          UserEnv{userAddress= UserAddress{listPayAddresses=addr:_}} <-  getState <|> error "no UserEnv"
          return $ show addr

        POSTData amountToSend <- publish addrs $ minput "pay"  ("pay option: choose amount" ::String)
        moutput $ "Sending " ++ show  amountToSend ++ " lovelace a " ++ show recipientAddrStr
        
        txId <- payOne  recipientAddr  $ fromInteger amountToSend

        localIO $ putStrLn $ "¡Transacción enviada con éxito! TxId: " ++ show txId


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


-- test :: Bool
-- test =
--   let h = BS.pack "e15b0c18b423ad121ebadb0220f0850cad878dd0391abfadd3ae27b70f"
--   in toHexFromStakeCredential (fromHexToStakeCredential h) == h


data HELLO= HELLO deriving (Read,Show,Typeable,Generic,FromJSON,ToJSON,Default)

data WORLD= WORLD deriving (Read,Show,Typeable,Generic,FromJSON,ToJSON,Default)

instance Loggable WORLD
instance Loggable HELLO

distrib= keep $ initNode $ inputNodes <|> do
  local $ option ("g" ::String) "go"
  nodes <- local getNodes
  let node = nodes !! 1
  let node2= nodes !! 2
  r <- runAt node (local $ do ttr HELLO;return HELLO)  -- <> runAt node2 (local $ do ttr HELLO;return HELLO)
  ttr ("RECEIVED",r )




-- testWitness = do
--     let witnessesHex = "a10081825820e5610a36c8ee4f5b71ea37be5ce5318f8260aa0b87fa131188140dccea1e83e0584084f863011379a5c237101fe6ed353b1914e186ac791497dca7b614fa51891aff3a114f9546a8ba9bd10b66862c088335c155c93a1107f4291a7f3b887d0a9407"
--     let witnessBytes = case B16.decode $ TE.encodeUtf8 witnessesHex of
--             Left err   -> error ("Invalid hex in witnesses: " ++ err) 
--             Right bytes -> bytes

--     ttr ("deserializing witness set", witnessBytes)
--     let lbs = LBS.fromStrict witnessBytes

--     case decCBOR lbs of
--       Left err ->
--         error (show err)
--       Right (ws :: TxWits ConwayEra) ->
--         print ws
    -- case  deserialiseFromCBOR  (asType :: AsType (TxWits  ConwayEra)) witnessBytes of
    --     Left err -> error ("Error deserializing TxWitness: " ++ show err)
    --     Right txWitness -> ttr txWitness 

-- DEBUG: RTS monitor for post-mortem analysis (remove when done)
startRTSMonitor :: IO ()
startRTSMonitor = void $ forkIO $ withFile "/var/log/cardano-cloud-stats.log" AppendMode loop
  where
  loop h = do
    enabled <- GHC.Stats.getRTSStatsEnabled
    when enabled $ do
      s <- GHC.Stats.getRTSStats
      t <- getCurrentTime
      hPutStrLn h $ unwords
        [ show t
        , "live="    ++ show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s) `div` 1024) ++ "KB"
        , "max="     ++ show (GHC.Stats.max_live_bytes s `div` 1024) ++ "KB"
        , "alloc="   ++ show (GHC.Stats.allocated_bytes s `div` 1024 `div` 1024) ++ "MB"
        , "gcs="     ++ show (GHC.Stats.gcs s)
        ]
      hFlush h
    threadDelay 30000000
    loop h
-- END DEBUG

mainPay= do
  -- DEBUG: enable heap profiling by type (remove when done)
  -- run with: +RTS -l -hT -N
  -- generates: cardano-cloud-test.eventlog  (ghc-events/threadscope)
  --            cardano-cloud-test.hp        (hp2ps: heap by type, no -prof needed)
  startRTSMonitor
  runCC paymentExample

-- main= do
--   publishedURLs("handle wallet")
--   empty
--   some code...
--   publishURL "handle wallet" $ url "/getbalance" <|> url  "/get"

mainseq= keep $ initNode $ do
  runJobs
  minput "111" ("1111" :: String) :: Cloud()
  minput "222" ("2222" :: String) :: Cloud()
  minput "333" ("3333" :: String) :: Cloud()
  moutput ("4444" :: String)
  job
  localIO $ putStrLn "FIN"


main= keep $ initNode $ do
  runJobs
  uit  <|> return()
  publish "pub" $ minput "cont" ("cont" ::String):: Cloud String
  x <- local $ getState <|> error "NO STRING STATE"
  ttr ("STATE",x)
  moutput (x :: String)
  job

  
  ttr "AFTER JOB"
  where
  uit= do 
        minput "init" ("init1":: String) :: Cloud ()

        ttr "AFTER INIT"
        session <- local $ do
            mc <-getCookie "state"
            ttr ("COOKIEE",mc)
            if isJust mc
              then return $ fromJust mc
              else do
                s <- genSessionId
                setCookie "state" $ BSS.fromStrict s <> "; Path=/"
                return s
        onAll $ setSession session
        ttr "AFTER SESSION"

        x <- minput "uit" ("uit" :: String) 
        ttr "SETSTATE"
        setState (x :: String)
        moutput ("after setState":: String)
        local $ return ("PASOPORAQUI" :: String)
        local $ published "pub"
        ttr "AFTER PUBLISHED"
        empty

  


mainCookie= keep $ initNode $ do
  runJobs

  minput "init" ("init" ::String) :: Cloud ()

  local $ do
      session <- newSession
      setCookie "state" $ BSS.fromStrict session <> "; Path=/"

  minput "next" ("next" ::String) :: Cloud ()

  moutput ("END" ::String)

mainSequence= keep $ initNode $ do
  runJobs
  minput "init" ("init" :: String) :: Cloud()
  session <- local $ do
      mc <-getCookie "state"
      ttr ("COOKIEE",mc)
      if isJust mc
        then return $ fromJust mc
        else do
          s <- genSessionId
          setCookie "state" $ BSS.fromStrict s <> "; Path=/"
          return s
  onAll $ setSession session


  publish "General" $ minput "next" ("next" ::String) <|> (onAll (published "General") >> empty) :: Cloud ()

  publish "General" $ minput "next2" ("next2" ::String) :: Cloud ()
  job


  moutput ("END" :: String)




mainState= keep $ initNode $ do


  user <|> return () -- liftCloud fork user

  job
  publish "General" $ minput "next" ("next" ::String) :: Cloud ()

  minput "results" ("results" ::String) :: Cloud()

  x <- local $ getState <|> error "no state" :: Cloud (String,String)
  moutput ("END" :: String ,x ::(String,String)) :: Cloud ()
  where
  user= do
    minput "init" ("init" :: String):: Cloud()
    session <- local $ do
            mc <-getCookie "state"
            ttr ("COOKIEE",mc)
            if isJust mc
              then return $ fromJust mc
              else do
                s <- genSessionId
                setCookie "state" $ BSS.fromStrict s <> "; Path=/"
                return s
    onAll $ setSession session
    (x,y) <- minput "name" ("enter name" :: String)   :: Cloud (String,String)
    onAllNodes $ setState (x,y)
    local $ published  "General"
    empty






mainStream= keep $ initNode $ do
  minput "init" ("init" :: String):: Cloud()
  local iter
  where
  iter :: TransIO ()
  iter =   do
    n <- for [1..10]

    ttr n
    -- liftIO $ threadDelay 1000000
    output  (n :: Int)




mainbox= keep loop
  where
  loop = get <|> put

  get = do
   r <- getMailbox
   liftIO $ threadDelay 1000000
   ttr (r :: String)

  put = do
    abduce
    onWaitThreads $ const $ put
    putMailbox  ("hello" :: String)
    showAllThreads

mainx = runCC $ do
  UserEnv{} <- onAll  getState
  local $ iter 0

iter n=  do
    output $ show (n :: Int)
    liftIO $ threadDelay 1000000
    iter $ n + 1


mainstate= keep $ initNode $ do
  ( do
      minput "init" ("init" :: String) :: Cloud()
      onAll $ do
        session <- newSession
        setCookie "state" $ BSS.fromStrict session <> "; Path=/"
        setState ("HELLO" :: String,"WORLD" :: String)
      minput "init2" ("init2" :: String)  <|> onAll (published "hello" >> empty) :: Cloud()

      )  <|> return ()

  onAll $ tttr "BEFORE PUBLISH"
  publish "hello" $ minput "start" ("start" :: String) :: Cloud ()


  pair <- onAll $ getState  <|> error "no state"
  ttr (pair :: (String,String))

  moutput ("hello called" :: String,pair)
