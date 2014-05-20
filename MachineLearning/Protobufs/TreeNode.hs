{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MachineLearning.Protobufs.TreeNode (TreeNode(..)) where
import qualified Data.Data                            as Prelude'
import qualified Data.Typeable                        as Prelude'
import qualified MachineLearning.Protobufs.Annotation as Protobufs (Annotation)
import           Prelude                              ((+), (/))
import qualified Prelude                              as Prelude'
import qualified Text.ProtocolBuffers.Header          as P'

data TreeNode = TreeNode{feature :: !(P'.Maybe P'.Int64), splitValue :: !(P'.Maybe P'.Double), left :: !(P'.Maybe TreeNode),
                         right :: !(P'.Maybe TreeNode), leafValue :: !(P'.Maybe P'.Double),
                         annotation :: !(P'.Maybe Protobufs.Annotation)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable TreeNode where
  mergeAppend (TreeNode x'1 x'2 x'3 x'4 x'5 x'6) (TreeNode y'1 y'2 y'3 y'4 y'5 y'6)
   = TreeNode (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)

instance P'.Default TreeNode where
  defaultValue = TreeNode P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire TreeNode where
  wireSize ft' self'@(TreeNode x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 1 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 1 x'5
             + P'.wireSizeOpt 1 11 x'6)
  wirePut ft' self'@(TreeNode x'1 x'2 x'3 x'4 x'5 x'6)
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
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutOpt 41 1 x'5
             P'.wirePutOpt 50 11 x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{feature = Prelude'.Just new'Field}) (P'.wireGet 3)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{splitValue = Prelude'.Just new'Field}) (P'.wireGet 1)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{left = P'.mergeAppend (left old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{right = P'.mergeAppend (right old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             41 -> Prelude'.fmap (\ !new'Field -> old'Self{leafValue = Prelude'.Just new'Field}) (P'.wireGet 1)
             50 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{annotation = P'.mergeAppend (annotation old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TreeNode) TreeNode where
  getVal m' f' = f' m'

instance P'.GPB TreeNode

instance P'.ReflectDescriptor TreeNode where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 17, 26, 34, 41, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protobufs.TreeNode\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"TreeNode\"}, descFilePath = [\"MachineLearning\",\"Protobufs\",\"TreeNode.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.feature\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"feature\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.splitValue\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"splitValue\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.left\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"left\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.TreeNode\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"TreeNode\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.right\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"right\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.TreeNode\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"TreeNode\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.leafValue\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"leafValue\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protobufs.TreeNode.annotation\", haskellPrefix' = [MName \"MachineLearning\"], parentModule' = [MName \"Protobufs\",MName \"TreeNode\"], baseName' = FName \"annotation\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".protobufs.Annotation\", haskellPrefix = [MName \"MachineLearning\"], parentModule = [MName \"Protobufs\"], baseName = MName \"Annotation\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
