{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.GridFsConfig (GridFsConfig(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data GridFsConfig = GridFsConfig{database :: !(P'.Maybe P'.Utf8), collection :: !(P'.Maybe P'.Utf8), file :: !(P'.Maybe P'.Utf8)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GridFsConfig where
  mergeAppend (GridFsConfig x'1 x'2 x'3) (GridFsConfig y'1 y'2 y'3)
   = GridFsConfig (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default GridFsConfig where
  defaultValue = GridFsConfig P'.defaultValue (Prelude'.Just (P'.Utf8 (P'.pack "fs"))) P'.defaultValue
 
instance P'.Wire GridFsConfig where
  wireSize ft' self'@(GridFsConfig x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3)
  wirePut ft' self'@(GridFsConfig x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
             P'.wirePutOpt 18 9 x'2
             P'.wirePutOpt 26 9 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{database = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{collection = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{file = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GridFsConfig) GridFsConfig where
  getVal m' f' = f' m'
 
instance P'.GPB GridFsConfig
 
instance P'.ReflectDescriptor GridFsConfig where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.GridFsConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"GridFsConfig\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"GridFsConfig.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.GridFsConfig.database\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"GridFsConfig\"], baseName' = FName \"database\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.GridFsConfig.collection\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"GridFsConfig\"], baseName' = FName \"collection\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Just \"fs\", hsDefault = Just (HsDef'ByteString \"fs\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.GridFsConfig.file\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"GridFsConfig\"], baseName' = FName \"file\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"