{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.InfluenceTrimmingConfig (InfluenceTrimmingConfig(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data InfluenceTrimmingConfig = InfluenceTrimmingConfig{alpha :: !(P'.Maybe P'.Double), warmupRounds :: !(P'.Maybe P'.Int64)}
                             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable InfluenceTrimmingConfig where
  mergeAppend (InfluenceTrimmingConfig x'1 x'2) (InfluenceTrimmingConfig y'1 y'2)
   = InfluenceTrimmingConfig (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default InfluenceTrimmingConfig where
  defaultValue = InfluenceTrimmingConfig P'.defaultValue P'.defaultValue
 
instance P'.Wire InfluenceTrimmingConfig where
  wireSize ft' self'@(InfluenceTrimmingConfig x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 3 x'2)
  wirePut ft' self'@(InfluenceTrimmingConfig x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 9 1 x'1
             P'.wirePutOpt 16 3 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{alpha = Prelude'.Just new'Field}) (P'.wireGet 1)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{warmupRounds = Prelude'.Just new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> InfluenceTrimmingConfig) InfluenceTrimmingConfig where
  getVal m' f' = f' m'
 
instance P'.GPB InfluenceTrimmingConfig
 
instance P'.ReflectDescriptor InfluenceTrimmingConfig where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.InfluenceTrimmingConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"InfluenceTrimmingConfig\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"InfluenceTrimmingConfig.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.InfluenceTrimmingConfig.alpha\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"InfluenceTrimmingConfig\"], baseName' = FName \"alpha\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.InfluenceTrimmingConfig.warmupRounds\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"InfluenceTrimmingConfig\"], baseName' = FName \"warmupRounds\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"