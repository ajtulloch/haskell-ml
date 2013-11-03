{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.DataSource (DataSource(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data DataSource = GRIDFS
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable DataSource
 
instance Prelude'.Bounded DataSource where
  minBound = GRIDFS
  maxBound = GRIDFS
 
instance P'.Default DataSource where
  defaultValue = GRIDFS
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe DataSource
toMaybe'Enum 1 = Prelude'.Just GRIDFS
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum DataSource where
  fromEnum GRIDFS = 1
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type MachineLearning.Protobufs.DataSource") .
      toMaybe'Enum
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type MachineLearning.Protobufs.DataSource"
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type MachineLearning.Protobufs.DataSource"
 
instance P'.Wire DataSource where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB DataSource
 
instance P'.MessageAPI msg' (msg' -> DataSource) DataSource where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum DataSource where
  reflectEnum = [(1, "GRIDFS", GRIDFS)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protobufs.DataSource") ["MachineLearning"] ["Protobufs"] "DataSource")
      ["MachineLearning", "Protobufs", "DataSource.hs"]
      [(1, "GRIDFS")]