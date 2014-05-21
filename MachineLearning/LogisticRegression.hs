{-|
Module      : MachineLearning.LogisticRegression
Description : Gradient Boosting and Random Forests
Copyright   : (c) Andrew Tulloch, 2014
License     : MIT
Maintainer  : andrew@tullo.ch
Stability   : experimental
Portability : POSIX

Implements simple logistic regression
 -}
module MachineLearning.LogisticRegression
    (-- * Types
     State(..),
     -- * Training
     train,
     -- * Prediction
     predict)
where

import           Data.Maybe                        (fromJust)
import           Data.Vector                       as V
import qualified MachineLearning.Protobufs.Example as PB
import           MachineLearning.Util

-- | Maintains the state for logistic regression models.
data State = State {
      _weights      :: V.Vector Double
    , _learningRate :: Double
    } deriving (Show, Eq, Read)


-- | Updates the logistic regression model with a new example.
train :: PB.Example -> State -> State
train example oldState =
    State { _weights=newWeights
          , _learningRate=newLearningRate
          }
  where
    newWeights = computeUpdate oldState example
    newLearningRate = _learningRate oldState

-- | Computes the prediction for the trained model on the given
predict ::  State -> V.Vector Double -> Double
predict state features = 1.0 / (1.0 + exp (-1 * logit))
  where
    logit = dotProduct features (_weights state)

gradients :: State -> PB.Example -> Vector Double
gradients state example =
    fmap (\x -> _learningRate state * gradient * x) (features' example)
  where
    gradient = fromJust (PB.label example) -  prediction
    prediction = predict state (features' example)

computeUpdate :: State -> PB.Example -> Vector Double
computeUpdate state example =
    V.zipWith (+) (gradients state example) (_weights state)
