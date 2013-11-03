{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.LossFunctionConfig (LossFunctionConfig(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified MachineLearning.Protobufs.LossFunction as Protobufs (LossFunction)
 
data LossFunctionConfig = LossFunctionConfig{lossFunction :: !(P'.Maybe Protobufs.LossFunction),
                                             huberAlpha :: !(P'.Maybe P'.Double)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable LossFunctionConfig where
  mergeAppend (LossFunctionConfig x'1 x'2) (LossFunctionConfig y'1 y'2)
   = LossFunctionConfig (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default LossFunctionConfig where
  defaultValue = LossFunctionConfig P'.defaultValue P'.defaultValue
 
instance P'.Wire LossFunctionConfig where
  wireSize ft' self'@(LossFunctionConfig x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 1 x'2)
  wirePut ft' self'@(LossFunctionConfig x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 17 1 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{lossFunction = Prelude'.Just new'Field}) (P'.wireGet 14)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{huberAlpha = Prelude'.Just new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> LossFunctionConfig) LossFunctionConfig where
  getVal m' f' = f' m'
 
instance P'.GPB LossFunctionConfig
 
instance P'.ReflectDescriptor LossFunctionConfig where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 17])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.LossFunctionConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"LossFunctionConfig\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"LossFunctionConfig.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.LossFunctionConfig.lossFunction\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"LossFunctionConfig\"], baseName' = FName \"lossFunction\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.LossFunction\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"LossFunction\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.LossFunctionConfig.huberAlpha\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"LossFunctionConfig\"], baseName' = FName \"huberAlpha\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"