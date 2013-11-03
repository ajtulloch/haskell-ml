module MachineLearning.LogisticRegression
    (State(..),
     train,
    )
where

import qualified Data.Foldable                     as F
import           Data.Maybe                        (fromJust)
import qualified Data.Sequence                     as S
import qualified MachineLearning.Protobufs.Example as PB

data State = State {
      weights      :: S.Seq Double
    , learningRate :: Double
    } deriving (Show, Eq, Read)

dotProduct :: Num b => S.Seq b -> S.Seq b -> b
dotProduct x y = F.foldl (+) 0 (S.zipWith (*) x y)

train :: PB.Example -> State -> State
train example oldState =  State { weights=newWeights
                                , learningRate=newLearningRate
                                } where
    newWeights = computeUpdate example oldState
    newLearningRate = learningRate oldState

predict :: PB.Example -> State -> Double
predict example state = 1.0 / (1.0 + exp (-1 * logit)) where
    logit = dotProduct (PB.features example) (weights state)

gradients :: PB.Example -> State -> S.Seq Double
gradients example state =
    fmap (\x -> learningRate state * update * x) (PB.features example) where
        update = fromJust (PB.label example) -  prediction
        prediction = predict example state

computeUpdate :: PB.Example -> State -> S.Seq Double
computeUpdate example state = S.zipWith (+) (gradients example state) (weights state)
