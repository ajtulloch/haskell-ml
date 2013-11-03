{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.TrainingStatus (TrainingStatus(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data TrainingStatus = UNCLAIMED
                    | PROCESSING
                    | FINISHED
                    deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TrainingStatus
 
instance Prelude'.Bounded TrainingStatus where
  minBound = UNCLAIMED
  maxBound = FINISHED
 
instance P'.Default TrainingStatus where
  defaultValue = UNCLAIMED
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe TrainingStatus
toMaybe'Enum 1 = Prelude'.Just UNCLAIMED
toMaybe'Enum 2 = Prelude'.Just PROCESSING
toMaybe'Enum 3 = Prelude'.Just FINISHED
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum TrainingStatus where
  fromEnum UNCLAIMED = 1
  fromEnum PROCESSING = 2
  fromEnum FINISHED = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type MachineLearning.Protobufs.TrainingStatus") .
      toMaybe'Enum
  succ UNCLAIMED = PROCESSING
  succ PROCESSING = FINISHED
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type MachineLearning.Protobufs.TrainingStatus"
  pred PROCESSING = UNCLAIMED
  pred FINISHED = PROCESSING
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type MachineLearning.Protobufs.TrainingStatus"
 
instance P'.Wire TrainingStatus where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB TrainingStatus
 
instance P'.MessageAPI msg' (msg' -> TrainingStatus) TrainingStatus where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum TrainingStatus where
  reflectEnum = [(1, "UNCLAIMED", UNCLAIMED), (2, "PROCESSING", PROCESSING), (3, "FINISHED", FINISHED)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protobufs.TrainingStatus") ["MachineLearning"] ["Protobufs"] "TrainingStatus")
      ["MachineLearning", "Protobufs", "TrainingStatus.hs"]
      [(1, "UNCLAIMED"), (2, "PROCESSING"), (3, "FINISHED")]