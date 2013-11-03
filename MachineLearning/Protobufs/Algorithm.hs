{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.Algorithm (Algorithm(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Algorithm = BOOSTING
               | RANDOM_FOREST
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Algorithm
 
instance Prelude'.Bounded Algorithm where
  minBound = BOOSTING
  maxBound = RANDOM_FOREST
 
instance P'.Default Algorithm where
  defaultValue = BOOSTING
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Algorithm
toMaybe'Enum 1 = Prelude'.Just BOOSTING
toMaybe'Enum 2 = Prelude'.Just RANDOM_FOREST
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Algorithm where
  fromEnum BOOSTING = 1
  fromEnum RANDOM_FOREST = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type MachineLearning.Protobufs.Algorithm") .
      toMaybe'Enum
  succ BOOSTING = RANDOM_FOREST
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type MachineLearning.Protobufs.Algorithm"
  pred RANDOM_FOREST = BOOSTING
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type MachineLearning.Protobufs.Algorithm"
 
instance P'.Wire Algorithm where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Algorithm
 
instance P'.MessageAPI msg' (msg' -> Algorithm) Algorithm where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Algorithm where
  reflectEnum = [(1, "BOOSTING", BOOSTING), (2, "RANDOM_FOREST", RANDOM_FOREST)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protobufs.Algorithm") ["MachineLearning"] ["Protobufs"] "Algorithm")
      ["MachineLearning", "Protobufs", "Algorithm.hs"]
      [(1, "BOOSTING"), (2, "RANDOM_FOREST")]