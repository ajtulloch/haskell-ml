{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.SplittingConstraints (SplittingConstraints(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data SplittingConstraints = SplittingConstraints{maximumLevels :: !(P'.Maybe P'.Int64), minimumAverageGain :: !(P'.Maybe P'.Double),
                                                 minimumSamplesAtLeaf :: !(P'.Maybe P'.Int64)}
                          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SplittingConstraints where
  mergeAppend (SplittingConstraints x'1 x'2 x'3) (SplittingConstraints y'1 y'2 y'3)
   = SplittingConstraints (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default SplittingConstraints where
  defaultValue = SplittingConstraints P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire SplittingConstraints where
  wireSize ft' self'@(SplittingConstraints x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 1 x'2 + P'.wireSizeOpt 1 3 x'3)
  wirePut ft' self'@(SplittingConstraints x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 3 x'1
             P'.wirePutOpt 17 1 x'2
             P'.wirePutOpt 24 3 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{maximumLevels = Prelude'.Just new'Field}) (P'.wireGet 3)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{minimumAverageGain = Prelude'.Just new'Field}) (P'.wireGet 1)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{minimumSamplesAtLeaf = Prelude'.Just new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SplittingConstraints) SplittingConstraints where
  getVal m' f' = f' m'
 
instance P'.GPB SplittingConstraints
 
instance P'.ReflectDescriptor SplittingConstraints where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 17, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.SplittingConstraints\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"SplittingConstraints\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"SplittingConstraints.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.SplittingConstraints.maximumLevels\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"SplittingConstraints\"], baseName' = FName \"maximumLevels\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.SplittingConstraints.minimumAverageGain\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"SplittingConstraints\"], baseName' = FName \"minimumAverageGain\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.SplittingConstraints.minimumSamplesAtLeaf\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"SplittingConstraints\"], baseName' = FName \"minimumSamplesAtLeaf\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"