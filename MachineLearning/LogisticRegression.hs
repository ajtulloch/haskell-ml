module MachineLearning.LogisticRegression
    (State(..),
     train,
     predict)
where

import qualified Data.Foldable                     as F
import           Data.Maybe                        (fromJust)
import qualified Data.Sequence                     as S
import qualified MachineLearning.Protobufs.Example as PB

data State = State {
      _weights      :: S.Seq Double
    , _learningRate :: Double
    } deriving (Show, Eq, Read)

dotProduct :: Num b => S.Seq b -> S.Seq b -> b
dotProduct x y = F.foldl (+) 0 (S.zipWith (*) x y)

train :: PB.Example -> State -> State
train example oldState =
    State { _weights=newWeights
          , _learningRate=newLearningRate
          }
  where
    newWeights = computeUpdate oldState example
    newLearningRate = _learningRate oldState

predict ::  State -> PB.Example -> Double
predict state example = 1.0 / (1.0 + exp (-1 * logit))
  where
    logit = dotProduct (PB.features example) (_weights state)

gradients :: State -> PB.Example -> S.Seq Double
gradients state example =
    fmap (\x -> _learningRate state * update * x) (PB.features example)
  where
    update = fromJust (PB.label example) -  prediction
    prediction = predict state example

computeUpdate :: State -> PB.Example -> S.Seq Double
computeUpdate state example =
    S.zipWith (+) (gradients state example) (_weights state)
