module MachineLearning.LogisticRegression where

import MachineLearning.Common

import Data.Vector as V
import qualified Data.List
import Data.Function

data LogisticRegressionState = LogisticRegressionState {
      weights :: V.Vector Double
    , learningRate :: Double
    } deriving (Show, Eq, Read)

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct a b = V.sum $ V.zipWith (*) a b

onlineLogisticRegression :: Example -> LogisticRegressionState -> LogisticRegressionState
onlineLogisticRegression example oldState = newState where
    newState = LogisticRegressionState {weights=newWeights, learningRate=newLearningRate}
    newWeights = computeUpdate example oldState
    newLearningRate = learningRate oldState

predict :: Example -> LogisticRegressionState -> Double
predict example state = 1.0 / (1.0 + exp (-1 * logit)) where
    logit = dotProduct (features example) (weights state)

gradients :: Example -> LogisticRegressionState -> Vector Double
gradients example state =
    V.map (\x -> learningRate state * update * x) (features example) where
        update = label example - prediction
        prediction = predict example state

computeUpdate :: Example -> LogisticRegressionState -> Vector Double
computeUpdate example state = V.zipWith (+) (gradients example state) (weights state)

batchLogisticRegression :: Vector Example -> Int -> LogisticRegressionState -> LogisticRegressionState
batchLogisticRegression _ 0 state = state
batchLogisticRegression  examples n state =
    batchLogisticRegression examples (n - 1) newState where
        newState = runBatchRound examples state

runBatchRound ::  Vector Example -> LogisticRegressionState -> LogisticRegressionState
runBatchRound examples initialState = LogisticRegressionState {
                                        learningRate=learningRate initialState,
                                        weights=weights
                                      } where
    weights = V.map (* scalingFactor) accumulatedGradients
    scalingFactor = 1.0 / fromIntegral (V.length examples)
    accumulatedGradients = V.foldl (V.zipWith (+)) emptyVector exampleGradients
    exampleGradients = V.map (`gradients` initialState) examples
    emptyVector = V.replicate (V.length examples) 0
