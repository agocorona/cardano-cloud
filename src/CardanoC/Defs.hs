{-# LANGUAGE DeriveAnyClass,DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, OverloadedStrings,UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}

module CardanoC.Defs where -- (UserAddress(..),UserEnv(..),AppEnv(..),ask) where

import Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Binary as CB
import qualified Cardano.Ledger.Api as LedgerApi
-- import qualified Codec.CBOR.Decoding as CBOR

-- import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Binary as L
import qualified Cardano.Ledger.Conway as LedgerConway
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM (TVar, newTVarIO, modifyTVar, STM)
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson hiding (Value)
import qualified Data.Aeson (Value)
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Coerce
import Data.Default
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.TCache
import Data.TCache.Defs
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics
import qualified Text.Parsec as Parsec
import Transient.Base
import Transient.Loggable
import Transient.Move.JSON
import Transient.Move

data UserAddress = 
   UserAddress
    {listPayAddresses :: [AddressInEra ConwayEra]
    ,listUnusedAddresses :: [AddressInEra ConwayEra]
    ,changeAddress :: Maybe(AddressInEra ConwayEra)
    ,listStakeAddresses :: [Creed]
    ,walletUtxos :: [T.Text]
    }  deriving (Default,Generic,Typeable) -- (Default,Typeable)



type WalletUtxo =  T.Text 
    -- { utxoIn   :: TxIn
    -- , utxoHex  :: T.Text -- We store the raw Hex as it comes
    -- } deriving (Show, Generic)

-- These instances are now trivial and work 100%
-- instance FromJSON WalletUtxo
-- instance ToJSON WalletUtxo


-- unpackUtxo :: WalletUtxo -> TxOutInAnyEra
-- unpackUtxo (WalletUtxo w) = 
--   let bsStrict = Base16.decodeLenient $ T.encodeUtf8  w
--       bsLazy = LBS.fromStrict bsStrict
--   in case Cardano.Binary.decodeFull bsLazy of
--        Left err -> error $ "Error decodeFull: " ++ show err
--        Right txOutLedger -> 
--          let apiOut :: TxOut CtxUTxO ConwayEra
--              apiOut = fromShelleyTxOut ShelleyBasedEraConway txOutLedger
--          in TxOutInAnyEra ConwayEra (toCtxTx ShelleyBasedEraConway apiOut)

-- toCtxTx :: ShelleyBasedEra era -> TxOut CtxUTxO era -> TxOut CtxTx era
--   toCtxTx _ (TxOut addr val dat ref) = 
--     TxOut addr val (convertDatum dat) ref
--     where
--       convertDatum d = case d of
--         TxOutDatumNone        -> TxOutDatumNone
--         TxOutDatumHash s h    -> TxOutDatumHash s h
--         TxOutDatumInline s sd -> TxOutDatumInline s sd

unpackUtxo :: WalletUtxo -> (TxIn, TxOutInAnyEra)
unpackUtxo  w = 
  let bs = Base16.decodeLenient $ T.encodeUtf8 w
      -- We use protocol version 10 (corresponding to Conway)
      version = L.natVersion @10 
  in case L.decodeFull version (LBS.fromStrict bs) of
       Left err -> error $ "Error decoding UTxO: " ++ show err
       -- The Ledger already knows how to decode the tuple (TxIn, TxOut) automatically
       Right (txInLedger, txOutLedger) -> 
         let txIn = fromShelleyTxIn txInLedger
             apiOut = fromShelleyTxOut ShelleyBasedEraConway txOutLedger
         in (txIn, TxOutInAnyEra ConwayEra (toCtxTx ShelleyBasedEraConway apiOut))
  where
  -- Clear type signature
  toCtxTx :: ShelleyBasedEra era -> TxOut CtxUTxO era -> TxOut CtxTx era
  -- Implementation with clean indentation
  toCtxTx _ (TxOut addr val dat ref) = 
      TxOut addr val (convertDatum dat) ref
    where
      convertDatum :: TxOutDatum CtxUTxO era -> TxOutDatum CtxTx era
      convertDatum d = case d of
        TxOutDatumNone        -> TxOutDatumNone
        TxOutDatumHash s h    -> TxOutDatumHash s h
        TxOutDatumInline s sd -> TxOutDatumInline s sd

newtype MPool= MPool (SlotNo, TxInMode)


-- 6. currentSlot → Directly uses getLocalChainTip with envConn
currentSlot :: TransIO (ChainTip,SlotNo)
currentSlot = do
  env <- ask
  tip <- liftIO $ getLocalChainTip (envConn env)
  return $ case tip of
    ChainTipAtGenesis -> (tip,SlotNo 0)
    ChainTip slotNo _hash _blockNo -> (tip,slotNo)

toMiUTxO :: TxIn -> Creed -> SlotNo -> TxOutInAnyEra -> MiUTxO
toMiUTxO txIn ownerCred slotCreated
         (TxOutInAnyEra _era (TxOut addrInEra txOutVal txOutDatum refScriptField)) =
  case addrInEra of
    AddressInEra _ addr ->
      MiUTxO
        { utxoId    = txIn
        , addr      = toAddressAny addr
        , value     = txOutValueToValue txOutVal
        , datum     = extractDatum txOutDatum
        , datumHash = extractDatumHash txOutDatum
        , refScript = extractRefScript refScriptField
        , slotNo    = slotCreated
        , spent     = Unspent
        , owner     = ownerCred
        }
  where
    extractDatum :: TxOutDatum CtxTx era -> Maybe HashableScriptData
    extractDatum d =
      case d of
        TxOutDatumNone               -> Nothing
        TxOutDatumHash _ _           -> Nothing
        TxOutSupplementalDatum _ hs  -> Just hs
        TxOutDatumInline _ hs        -> Just hs

    extractDatumHash :: TxOutDatum ctx era -> Maybe (Hash ScriptData)
    extractDatumHash d =
      case d of
        TxOutDatumHash _ h -> Just h
        _                  -> Nothing

    extractRefScript :: ReferenceScript era -> Maybe ScriptInAnyLang
    extractRefScript rs =
      case rs of
        ReferenceScriptNone    -> Nothing
        ReferenceScript _ sAny -> Just sAny



        
-- | Helper to extract the hash bytes regardless of the credential type
getCredentialBytes :: StakeCredential -> BS.ByteString
getCredentialBytes (StakeCredentialByKey h)   = serialiseToRawBytes h
getCredentialBytes (StakeCredentialByScript h) = serialiseToRawBytes h

deriving instance Generic (Hash StakeKey)

deriving instance FromJSON (Hash StakeKey)

deriving instance Generic StakeCredential 

deriving instance FromJSON StakeCredential 

newtype Creed= Creed Cardano.Api.StakeCredential -- deriving(Generic,FromJSON,ToJSON)

instance ToJSON Creed where
  toJSON (Creed credencial) = case credencial of
    StakeCredentialByKey h -> 
      object [ "stakingKeyHash" .= serialiseToRawBytesHexText h ]
    
    StakeCredentialByScript sh -> 
      -- Instead of 'error', we store the script hash so it doesn't crash
      object [ "stakingScriptHash" .= serialiseToRawBytesHexText sh ]

instance FromJSON Creed where
  parseJSON = withObject "Creed" $ \v -> do
    -- We try to look for either of the two keys
    mKey <- v .:? "stakingKeyHash"
    mScript <- v .:? "stakingScriptHash"
    case (mKey, mScript) of
      (Just hex, _) -> 
        case deserialiseFromRawBytesHex (T.encodeUtf8 hex) of
          Right h -> return $ Creed (StakeCredentialByKey h)
          Left err -> fail $ "Invalid key hash: " ++ show err
      
      (_, Just hex) -> 
        case deserialiseFromRawBytesHex (T.encodeUtf8 hex) of
          Right sh -> return $ Creed (StakeCredentialByScript sh)
          Left err -> fail $ "Invalid script hash: " ++ show err
          
      _ -> fail "Neither stakingKeyHash nor stakingScriptHash found"


-- | Instance that uses String logic to automatically add quotes
instance Show Creed where
  show (Creed s) = show (BSC.unpack (B16.encode (getCredentialBytes s)))

-- | Instance that uses String logic to automatically remove quotes
instance Read Creed where
  readsPrec d s = do
    (content, rest) <- (readsPrec d s :: [(String, String)])
    let rawBytes = fromRight BS.empty (B16.decode (BSC.pack content))
    if BS.length rawBytes == 28
      then case deserialiseFromRawBytes (AsHash AsStakeKey) rawBytes of
             Right h -> [(Creed (StakeCredentialByKey h), rest)]
             Left _  -> []
      else []



instance Eq Creed where
  (Creed c1) == (Creed c2) = 
    getCredentialBytes c1 == getCredentialBytes c2

instance Ord Creed where
  compare (Creed c1) (Creed c2) = 
    compare (getCredentialBytes c1) (getCredentialBytes c2)

data UserEnv = UserEnv{userAddress :: UserAddress }

instance Loggable UserAddress where
  serialize= serializeToJSON
  deserialize=deserializeJSON





-- instance Show UserAddress where
--   show x= BSL.unpack . B.toLazyByteString . serializeToJSON . toJSON $ x

-- instance Read UserAddress where
--   readsPrec _ _= error "read UserAddress: not implemented"

-- need this JSON instance because we need to serial/deserial from HEXadecimal wallet format for addresses
instance ToJSON UserAddress where
  toJSON ua = object
    [ "listPayAddresses"      .= fmap (bsToText . toHexFromConwayAddress) (listPayAddresses ua)
    , "listUnusedAddresses"   .= fmap (bsToText . toHexFromConwayAddress) (listPayAddresses ua)
    , "changeAddress"      .= fmap (bsToText . toHexFromConwayAddress) (changeAddress ua)
    , "listStakeAddresses" .= fmap (bsToText . toHexFromStakeCredential) (map coerce $ listStakeAddresses ua)
    , "walletUtxos" .= fmap (bsToText . LBS.toStrict . encode) ( walletUtxos ua)
    ]
    where
      bsToText = T.decodeUtf8

instance FromJSON UserAddress where
  parseJSON = withObject "UserAddress" $ \o ->
    UserAddress
      <$> fmap (map parseAddr)  (o .:  "listPayAddresses")
      <*> fmap (map parseAddr)  (o .: "listUnusedAddresses")
      <*> fmap (fmap parseAddr) (o .:? "changeAddress")
      <*> fmap (map parseStake) (o .:  "listStakeAddresses")
      <*> (o .:? "walletUtxos" .!= [])
    where
      parseAddr  = fromHexToConwayAddress . T.encodeUtf8
      parseStake = Creed . fromHexToStakeCredential . T.encodeUtf8
      -- parseUtxo  = either error id . eitherDecode . LBS.fromStrict . T.encodeUtf8 
      -- parseUtxo :: T.Text -> WalletUtxo
      -- parseUtxo t = either error id $ eitherDecode (LBS.fromStrict $ T.encodeUtf8 t)

        

deriving instance Generic LocalNodeConnectInfo
deriving instance FromJSON LocalNodeConnectInfo

deriving instance Generic NetworkId
deriving instance FromJSON NetworkId



deriving instance Generic ConsensusModeParams
deriving instance FromJSON ConsensusModeParams

-- deriving instance Generic EpochSlots
deriving instance FromJSON EpochSlots

deriving instance FromJSON NetworkMagic


-- newtype HashC a= HashC (Hash a)

-- deriving instance Eq (Hash a) => Eq (HashC a)
-- deriving instance Ord (Hash a) => Ord (HashC a)

-- | Show general para cualquier Hash que sepa serializarse a bytes
-- instance SerialiseAsRawBytes (Hash a) => Show (HashC a) where
--   show (HashC h) = show (BSC.unpack (B16.encode (serialiseToRawBytes h)))

-- | Read general usando el testigo de tipo (proxy)
-- instance (HasTypeProxy a, SerialiseAsRawBytes (Hash a)) => Read ( a) where
--   readsPrec d s = do
--     (content, rest) <- (readsPrec d s :: [(String, String)])
--     let rawBytes = fromRight BS.empty (B16.decode (BSC.pack content))
--     -- Usamos proxyToAsType para obtener el 'AsType a' que pide la API
--     case deserialiseFromRawBytes (AsHash (proxyToAsType Proxy)) rawBytes of
--       Right h -> [(HashC h, rest)]
--       Left _  -> []



data AppEnv = AppEnv
  { envConn       :: LocalNodeConnectInfo
  -- , envSigningKey :: SigningKey PaymentExtendedKey
  -- , envOwnAddress :: AddressInEra ConwayEra
  , envPParams    :: Ledger.PParams (ShelleyLedgerEra ConwayEra)  -- Key change here
  , envNetworkId  :: NetworkId
  , envEra        :: AnyCardanoEra
  } deriving(Show,Typeable)
  

-- 1) HEX -> AddressInEra ConwayEra
fromHexToConwayAddress
  :: BS.ByteString
  -> AddressInEra ConwayEra
fromHexToConwayAddress hex =
  case deserialiseFromRawBytesHex hex of
    Right addr ->  addr
    Left  left -> error $ show left


-- 2) AddressInEra ConwayEra -> HEX
toHexFromConwayAddress
  :: AddressInEra ConwayEra
  -> BS.ByteString
toHexFromConwayAddress =
  serialiseToRawBytesHex



fromHexToStakeCredential :: BS.ByteString -> StakeCredential
fromHexToStakeCredential =
  go . either (error "invalid hex") id . B16.decode
 where
  go bs = case BS.uncons bs of
    -- stake key hash
    Just (0xe1, h) ->
      let kh = either (error . show) id
                 (deserialiseFromRawBytes (AsHash AsStakeKey) h)
      in StakeCredentialByKey kh

    -- script hash (lo dejas como ya lo tenías)
    Just (0xe0, h) ->
      let sh = either (error . show) id (deserialiseFromRawBytes AsScriptHash h)
      in StakeCredentialByScript sh

    _ -> error "invalid stake credential" 


toHexFromStakeCredential :: StakeCredential -> BS.ByteString
toHexFromStakeCredential (StakeCredentialByKey kh) =
  B16.encode $ BS.cons 0xe1 (serialiseToRawBytes kh)
toHexFromStakeCredential (StakeCredentialByScript sh) =
  B16.encode $ BS.cons 0xe0 (serialiseToRawBytes sh)


ask= Transient.Base.getState <|> error ("cardano cloud: no state") :: TransIO AppEnv


instance Default StakeCredential where 
  def = StakeCredentialByKey dummyHash
    where
      dummyHash =
        case B16.decode "1c3a7b0d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d" of
          Right bs ->
            case deserialiseFromRawBytes (AsHash AsStakeKey) bs of
              Right h  -> h
              Left err -> error $ show err
          Left e -> error ("malformed hex: " <> e)



instance Default (AddressInEra ConwayEra) where
  def =
    makeShelleyAddressInEra
      (shelleyBasedEra @ConwayEra) -- era
      Mainnet                      -- network
      (PaymentCredentialByKey dummyHash) -- dummy payment
      NoStakeAddress
    where
      dummyHash =
        case deserialiseFromRawBytesHex
               "0000000000000000000000000000000000000000000000000000000000000000" of
          Right h -> h
          Left e  -> error ("invalid dummy hash: " )


-- instance FromJSON (KeyWitness ConwayEra) where
--   parseJSON = withText "KeyWitness ConwayEra" $ \t ->
--     case B64.decode (T.encodeUtf8 t) of
--       Right bs -> case makeShelleyKeyWitness   bs of
--                     Right kw -> pure kw
--                     Left err -> fail ("Failed to deserialise KeyWitness: " ++ show err)
--       Left err -> fail ("Invalid Base64: " ++ err)

-- instance ToJSON (KeyWitness ConwayEra) where
--   toJSON kw =
--     let bs = serialiseToRawBytes kw        -- KeyWitness -> ByteString
--         b64 = B64.encode bs                -- Base64
--         txt = T.decodeUtf8 b64            -- Text
--     in String txt


-- instance {-# OVERLAPPABLE #-}  (Read a, Show a, Typeable a,SerialiseAsCBOR a) => Loggable  a where
--    serialize  c   = byteString $  serialiseToCBOR c  `BS.snoc`   (0xFF :: Word8)
--    deserialize = r where
--      r = do
--       either <-  deserialiseFromCBOR (asType :: AsType  a)  <$> BS.toStrict <$> tTakeWhile' (/= '\xFF') 
--       case either of
--             Left err -> error $ "Error deserializing  " ++ show (typeOf (undefined :: a)) ++ "" ++show err
--             Right x -> return  x



-- instance FromJSON UserAddress where
--   parseJSON = withObject "UserAddress" $ \o ->
--     UserAddress
--       <$> fmap (map parseAddr) (o .:  "listPayAddresses")
--       <*> fmap (fmap parseAddr) (o .:? "changeAddress")
--       <*> fmap (map parseStake) (o .:  "listStakeAddresses")
--     where
--       parseAddr  = fromHexToConwayAddress . T.encodeUtf8
--       parseStake = fromHexToStakeCredential . T.encodeUtf8



-- deriving instance Generic StakeCredential                          
-- -- deriving instance FromJSON StakeCredential

-- instance FromJSON StakeCredential where
--   parseJSON = withText "StakeCredential" $ \txt ->
--     let bsHex = T.encodeUtf8 txt
--     in case B16.decode bsHex of
--          Right bs -> 
--            case deserialiseFromRawBytesHex bs of
--              Right h -> pure (StakeCredentialByKey h)
--              Left e  -> fail ("Invalid StakeCredential hash: " <> show e)
--          Left e -> fail ("StakeCredential hex mal formado: " <> e)




-- instance Loggable UserAddress where
--   serialize= serializeToJSON
--   deserialize=deserializeJSON

-- -- instance Default UserAddress where
-- --   def=  UserAddress{payAddresses=["$address1","$address2"] ,changeAddresses =["$changeaddres"] ,stakeAddresses=".."}
-- -- instance Generic UserAddress


-- | Instancia manual para deserializar una clave de texto hexadecimal a un Hash.
--   Esto es necesario para que `FromJSON` funcione con `Map (Hash PaymentKey) v`.
instance FromJSONKey (Hash PaymentKey) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case deserialiseFromRawBytesHex  (T.encodeUtf8 t) of
      Right h  -> pure h
      Left err -> fail $ "No se pudo deserializar la clave de pago del hash: " ++ show err

deriving instance Generic PaymentKey
deriving instance FromJSONKey PaymentKey
deriving instance FromJSON PaymentKey
deriving instance ToJSONKey  PaymentKey
deriving instance ToJSON  PaymentKey

deriving instance Ord PaymentKey
deriving instance Eq PaymentKey

data WatchList = WatchList 
  { credentials   :: Set.Set Creed  
  , vPaymentKeys :: Map.Map (Hash PaymentKey) Creed 
  } deriving (Generic,FromJSON,ToJSON) 

-- instance Serializable WatchList where
--     serialize  = LBS.pack . show
--     deserialize= read . LBS.unpack

instance  Serializable WatchList where
  serialize= Data.Aeson.encode
  deserialize s= fromMaybe (error $"deserialize decoding:"<> LBS.unpack s) $ Data.Aeson.decode s


instance Indexable WatchList where
  key _ = "app_watch_list" -- Unique key to find the file
  defPath _= "syncDB/"

watchList= getDBRef "app_watch_list" :: DBRef WatchList

{-# NOINLINE myPendingTxIds #-}
myPendingTxIds :: TVar (Set.Set TxId)
myPendingTxIds = unsafePerformIO $ newTVarIO Set.empty

trackTx :: TxId -> IO ()
trackTx txId = atomically $ modifyTVar myPendingTxIds (Set.insert txId)

untrackTx :: TxId -> STM ()
untrackTx txId = modifyTVar myPendingTxIds (Set.delete txId)


data TxEvent = TxInMempool TxId SlotNo | TxConfirmed TxId SlotNo
  deriving (Show, Generic, ToJSON, FromJSON)

data UTxOStatus
  = Unspent                -- Confirmado en Ledger (Bloque)
  | InMempool SlotNo       -- Intento de gasto o recepción (Radar)
  | Spent SlotNo           -- Gastado y confirmado (Bloque)
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data MiUTxO = MiUTxO 
  { utxoId    :: TxIn
  , addr      :: AddressAny
  , value     :: Cardano.Api.Value
  -- We store the datum as the "ScriptData" type which is common to all Shelley eras
  , datum     :: Maybe HashableScriptData 
  , datumHash :: Maybe (Hash ScriptData)
  -- We store the script in any language (Plutus V1/V2/V3 or Simple)
  , refScript :: Maybe (ScriptInAnyLang) 
  , slotNo    :: SlotNo
  , spent     :: UTxOStatus -- Maybe SlotNo
  , owner     :: Creed
  } deriving (Show, Eq)

-- data UTxOStatus 
--   = Unspent                -- Confirmed in Ledger (Block)
--   | InMempool SlotNo       -- Spend or receive attempt (Radar)
--   | Spent SlotNo           -- Spent and confirmed (Block)
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- to clean the spent, old utxos from the cache
-- reaper :: SlotNo -> Cloud ()
-- reaper currentSlot = do
--   -- We look for those that have been in "limbo" for too long
--   expiredRefs <- cloud $ liftIO $ atomically $ 
--                    select MiUTxO { utxoStatus = .<. InMempool (currentSlot - 20) }

--   mapM_ (restaurarOEliminar) expiredRefs

-- restaurarOEliminar :: DBRef MiUTxO -> Cloud ()
-- restaurarOEliminar ref = do
--   utxo <- cloud $ liftIO $ readDBRef ref
--   case utxoStatus utxo of
--     InMempool s -> do
--       -- Does it come from a TX we already knew (Unspent) or is it new?
--       -- If it is a UTXO we already had and it was only "blocked" by the mempool:
--       cloud $ liftIO $ writeResource utxo { utxoStatus = Unspent s }
--     _ -> return ()


-- instance ToJSON MiUTxO where
--   toJSON (MiUTxO uId ad val sl sp ow) = object
--     [ "utxoId" .= renderTxIn uId
--     , "addr"   .= serialiseAddress ad
--     , "value"  .= valueToJSON val
--     , "slotNo" .= sl
--     , "spent"  .= sp
--     , "owner"  .= ow
--     ]


instance ToJSON MiUTxO where
  toJSON mi = object 
    [ "utxoId"    .= renderTxIn (utxoId mi)
    , "addr"      .= serialiseAddress (addr mi)
    , "value"     .= valueToJSON (value mi)
    , "datum"     .= (case datum mi of
                        -- serialiseToCBOR returns pure bytes, then to Hex
                        Just d -> String $ T.decodeUtf8 $ Base16.encode $ serialiseToCBOR d
                        Nothing -> Null)
    , "datumHash" .= (serialiseToRawBytesHexText <$> datumHash mi)
    , "refScript" .= encodeRefScript (refScript mi)
    , "slotNo"    .= slotNo mi
    , "spent"     .= spent mi
    , "owner"     .= owner mi
    ]

encodeRefScript :: Maybe ScriptInAnyLang -> Data.Aeson.Value
encodeRefScript Nothing = Null
encodeRefScript (Just (ScriptInAnyLang lang script)) = object
  [ "lang"   .= show lang
  , "script" .= case script of
                  PlutusScript _ s -> serialiseToRawBytesHexText s

                  -- We serialize the complete GADT (Script SimpleScript'), not the content 's'
                  SimpleScript s -> T.decodeUtf8 $ Base16.encode $ serialiseToCBOR (SimpleScript s)
  ]


instance FromJSON MiUTxO where
  parseJSON = withObject "MiUTxO" $ \v -> do
    uId <- parseTxInJSON =<< v .: "utxoId"
    ad  <- parseAddrJSON =<< v .: "addr"
    
    mDatHex <- v .:? "datum"
    let mDat = case mDatHex of
                 Just h -> case Base16.decode (T.encodeUtf8 h) of
                             Right b -> case deserialiseFromCBOR AsHashableScriptData b of
                                          Right d -> Just d
                                          _       -> Nothing
                             _       -> Nothing
                 Nothing -> Nothing

    mHashHex <- v .:? "datumHash"
    let mHash = case mHashHex of
                  Just h -> case deserialiseFromRawBytesHex (T.encodeUtf8 h) of
                              Right h' -> Just h'
                              _        -> Nothing
                  Nothing -> Nothing

    mRefScript <- parseRefScriptJSON =<< v .:? "refScript" .!= Null

    MiUTxO uId ad 
      <$> v .: "value"
      <*> pure mDat
      <*> pure mHash
      <*> pure mRefScript
      <*> v .: "slotNo"
      <*> v .: "spent"
      <*> v .: "owner"

parseRefScriptJSON :: Data.Aeson.Value -> Parser (Maybe ScriptInAnyLang)
parseRefScriptJSON Null = return Nothing
parseRefScriptJSON (Object o) = do
  langStr <- o .: "lang" :: Parser T.Text
  hex     <- o .: "script" :: Parser T.Text
  let bHex = T.encodeUtf8 hex
  
  case langStr of
    "PlutusScriptLanguage PlutusScriptV3" -> 
      case deserialiseFromRawBytesHex bHex of
        Right s -> return $ Just (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) (PlutusScript PlutusScriptV3 s))
        _ -> fail "Error PlutusV3"
    "PlutusScriptLanguage PlutusScriptV2" -> 
      case deserialiseFromRawBytesHex bHex of
        Right s -> return $ Just (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) (PlutusScript PlutusScriptV2 s))
        _ -> fail "Error PlutusV2"
    "SimpleScriptLanguage" -> 
      case Base16.decode bHex of
        Right b -> 
          -- We use the AsScript proxy with the AsSimpleScript witness
          case deserialiseFromCBOR (AsScript AsSimpleScript) b of
            Right (SimpleScript s) -> 
              return $ Just (ScriptInAnyLang SimpleScriptLanguage (SimpleScript s))
            _ -> fail "Error deserializing CBOR of SimpleScript"
        _ -> fail "Error: Invalid script hexadecimal"
parseRefScriptJSON _ = return Nothing



-- | Parser for the address. Accepts Bech32 format (e.g., addr1...)
parseAddrJSON :: T.Text -> Data.Aeson.Types.Parser AddressAny
parseAddrJSON txt =
  case deserialiseAddress AsAddressAny txt of
    Just addrAny -> return addrAny
    Nothing      -> fail $ "Invalid Cardano address: " ++ T.unpack txt

-- | Parser for the TxIn (UTxO ID) using the API's internal Parsec parser
parseTxInJSON :: T.Text -> Data.Aeson.Types.Parser TxIn
parseTxInJSON txt =
  case Parsec.runParser parseTxIn () ""  txt of
    Right tid -> return tid
    Left err  -> fail $ "Invalid UTxO ID (expected format hash#index): " ++ show err















-- Corrected function for cardano-api 10.19.1.0
valueToJSON :: Cardano.Api.Value -> Data.Aeson.Value
valueToJSON v = toJSON $ valueToNestedRep v

emptyUtxo= let u= undefined in MiUTxO u u u u u u

instance Serializable MiUTxO where
  serialize= encode
  deserialize s= fromMaybe (error $ "deserialize decoding:  "<> LBS.unpack s) $ decode s


instance Read SlotNo where
  readsPrec d s = readParen (d > 10) (\r -> do
    ("SlotNo", rest) <- lex r
    (n, next)        <- readsPrec 11 rest
    return (SlotNo n, next)) s

-- deriving instance FromJSON SlotNo 

-- instance Show Addr where
--   show (Addr a)= T.unpack $ serialiseAddress a

-- instance Read Addr where
--   readsPrec _ ss =
--     let (s,rest)= fragment $ LBS.pack ss
--         mr = coerce $ deserialiseAddress (asType :: AsType AddressAny) $ T.decodeUtf8 $ LBS.toStrict s
--     in if isJust mr then [(fromJust mr, LBS.unpack rest)] else []

-- The UTxO ID (TxHash#Index) is the unique key
instance Indexable MiUTxO where
  key u = keyUtxo (utxoId u)
  defPath _= "syncDB/utxos/"

keyUtxo (TxIn txId (TxIx index))= T.unpack(serialiseToRawBytesHexText txId) ++ "#" ++ show index

-- loggedmsg :: String -> TransIO ()
loggedmsg s= void $ logged $ return (s :: String)
