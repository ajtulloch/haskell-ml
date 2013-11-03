{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.ForestConfig (ForestConfig(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified MachineLearning.Protobufs.Algorithm as Protobufs (Algorithm)
import qualified MachineLearning.Protobufs.InfluenceTrimmingConfig as Protobufs (InfluenceTrimmingConfig)
import qualified MachineLearning.Protobufs.LossFunctionConfig as Protobufs (LossFunctionConfig)
import qualified MachineLearning.Protobufs.ShrinkageConfig as Protobufs (ShrinkageConfig)
import qualified MachineLearning.Protobufs.SplittingConstraints as Protobufs (SplittingConstraints)
import qualified MachineLearning.Protobufs.StochasticityConfig as Protobufs (StochasticityConfig)
 
data ForestConfig = ForestConfig{numWeakLearners :: !(P'.Maybe P'.Int64),
                                 splittingConstraints :: !(P'.Maybe Protobufs.SplittingConstraints),
                                 lossFunctionConfig :: !(P'.Maybe Protobufs.LossFunctionConfig),
                                 influenceTrimmingConfig :: !(P'.Maybe Protobufs.InfluenceTrimmingConfig),
                                 shrinkageConfig :: !(P'.Maybe Protobufs.ShrinkageConfig),
                                 stochasticityConfig :: !(P'.Maybe Protobufs.StochasticityConfig),
                                 algorithm :: !(P'.Maybe Protobufs.Algorithm)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ForestConfig where
  mergeAppend (ForestConfig x'1 x'2 x'3 x'4 x'5 x'6 x'7) (ForestConfig y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = ForestConfig (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
 
instance P'.Default ForestConfig where
  defaultValue
   = ForestConfig P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ForestConfig where
  wireSize ft' self'@(ForestConfig x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 11 x'5
             + P'.wireSizeOpt 1 11 x'6
             + P'.wireSizeOpt 1 14 x'7)
  wirePut ft' self'@(ForestConfig x'1 x'2 x'3 x'4 x'5 x'6 x'7)
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
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutOpt 42 11 x'5
             P'.wirePutOpt 50 11 x'6
             P'.wirePutOpt 56 14 x'7
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{numWeakLearners = Prelude'.Just new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{splittingConstraints = P'.mergeAppend (splittingConstraints old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{lossFunctionConfig = P'.mergeAppend (lossFunctionConfig old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{influenceTrimmingConfig =
                                P'.mergeAppend (influenceTrimmingConfig old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{shrinkageConfig = P'.mergeAppend (shrinkageConfig old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             50 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{stochasticityConfig = P'.mergeAppend (stochasticityConfig old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{algorithm = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ForestConfig) ForestConfig where
  getVal m' f' = f' m'
 
instance P'.GPB ForestConfig
 
instance P'.ReflectDescriptor ForestConfig where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 34, 42, 50, 56])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.ForestConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"ForestConfig\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"ForestConfig.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.numWeakLearners\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"numWeakLearners\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.splittingConstraints\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"splittingConstraints\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.SplittingConstraints\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"SplittingConstraints\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.lossFunctionConfig\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"lossFunctionConfig\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.LossFunctionConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"LossFunctionConfig\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.influenceTrimmingConfig\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"influenceTrimmingConfig\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.InfluenceTrimmingConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"InfluenceTrimmingConfig\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.shrinkageConfig\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"shrinkageConfig\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.ShrinkageConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"ShrinkageConfig\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.stochasticityConfig\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"stochasticityConfig\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.StochasticityConfig\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"StochasticityConfig\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.ForestConfig.algorithm\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"ForestConfig\"], baseName' = FName \"algorithm\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.Algorithm\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"Algorithm\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"