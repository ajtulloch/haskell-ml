{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.LossFunction (LossFunction(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data LossFunction = LOGIT
                  | LEAST_ABSOLUTE_DEVIATION
                  | HUBER
                  deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable LossFunction
 
instance Prelude'.Bounded LossFunction where
  minBound = LOGIT
  maxBound = HUBER
 
instance P'.Default LossFunction where
  defaultValue = LOGIT
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe LossFunction
toMaybe'Enum 1 = Prelude'.Just LOGIT
toMaybe'Enum 2 = Prelude'.Just LEAST_ABSOLUTE_DEVIATION
toMaybe'Enum 3 = Prelude'.Just HUBER
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum LossFunction where
  fromEnum LOGIT = 1
  fromEnum LEAST_ABSOLUTE_DEVIATION = 2
  fromEnum HUBER = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type MachineLearning.Protobufs.LossFunction") .
      toMaybe'Enum
  succ LOGIT = LEAST_ABSOLUTE_DEVIATION
  succ LEAST_ABSOLUTE_DEVIATION = HUBER
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type MachineLearning.Protobufs.LossFunction"
  pred LEAST_ABSOLUTE_DEVIATION = LOGIT
  pred HUBER = LEAST_ABSOLUTE_DEVIATION
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type MachineLearning.Protobufs.LossFunction"
 
instance P'.Wire LossFunction where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB LossFunction
 
instance P'.MessageAPI msg' (msg' -> LossFunction) LossFunction where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum LossFunction where
  reflectEnum = [(1, "LOGIT", LOGIT), (2, "LEAST_ABSOLUTE_DEVIATION", LEAST_ABSOLUTE_DEVIATION), (3, "HUBER", HUBER)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protobufs.LossFunction") ["MachineLearning"] ["Protobufs"] "LossFunction")
      ["MachineLearning", "Protobufs", "LossFunction.hs"]
      [(1, "LOGIT"), (2, "LEAST_ABSOLUTE_DEVIATION"), (3, "HUBER")]