{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLearning.Protobufs.Rescaling (Rescaling(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Rescaling = NONE
               | AVERAGING
               | LOG_ODDS
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Rescaling
 
instance Prelude'.Bounded Rescaling where
  minBound = NONE
  maxBound = LOG_ODDS
 
instance P'.Default Rescaling where
  defaultValue = NONE
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Rescaling
toMaybe'Enum 1 = Prelude'.Just NONE
toMaybe'Enum 2 = Prelude'.Just AVERAGING
toMaybe'Enum 3 = Prelude'.Just LOG_ODDS
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Rescaling where
  fromEnum NONE = 1
  fromEnum AVERAGING = 2
  fromEnum LOG_ODDS = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type MachineLearning.Protobufs.Rescaling") .
      toMaybe'Enum
  succ NONE = AVERAGING
  succ AVERAGING = LOG_ODDS
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type MachineLearning.Protobufs.Rescaling"
  pred AVERAGING = NONE
  pred LOG_ODDS = AVERAGING
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type MachineLearning.Protobufs.Rescaling"
 
instance P'.Wire Rescaling where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Rescaling
 
instance P'.MessageAPI msg' (msg' -> Rescaling) Rescaling where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Rescaling where
  reflectEnum = [(1, "NONE", NONE), (2, "AVERAGING", AVERAGING), (3, "LOG_ODDS", LOG_ODDS)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protobufs.Rescaling") ["MachineLearning"] ["Protobufs"] "Rescaling")
      ["MachineLearning", "Protobufs", "Rescaling.hs"]
      [(1, "NONE"), (2, "AVERAGING"), (3, "LOG_ODDS")]