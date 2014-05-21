{-# LANGUAGE RecordWildCards #-}

{-|
Module      : MachineLearning.DecisionTrees
Description : Gradient Boosting and Random Forests
Copyright   : (c) Andrew Tulloch, 2014
License     : MIT
Maintainer  : andrew@tullo.ch
Stability   : experimental
Portability : POSIX

Decision Tree implements random forests and gradient boosting.
 -}
module MachineLearning.DecisionTrees (
     -- * Types
     Examples,
     -- * Loss Functions
     LossFunction(..),
     logitLoss,
     -- * Training
     trainBoosting,
     trainRandomForest,
     -- * Prediction
     predictForest) where

import           Data.Function                                  (on)
import           Data.List                                      (sortBy)
import           Data.Maybe                                     (fromJust,
                                                                 isJust)
import qualified Data.Sequence                                  as S
import           Data.Vector                                    ((!))
import qualified Data.Vector                                    as V

-- Protocol Buffer records
import qualified MachineLearning.Protobufs.Example              as PB
import qualified MachineLearning.Protobufs.SplittingConstraints as PB
import qualified MachineLearning.Protobufs.TreeNode             as PB
import           Text.ProtocolBuffers.Header                    (defaultValue)

import           Control.Monad
import           Control.Monad.Random
import           System.Random.Shuffle


data DecisionTree = Leaf {
      _value :: Double
    } | Branch {
      _feature :: Int
    , _value   :: Double
    , _left    :: DecisionTree
    , _right   :: DecisionTree
    }

data Split = Split {
      _splitFeature :: Int
    , _splitValue   :: Double
    , _averageGain  :: Double
    }

data LossState = LossState {
      _averageLabel         :: Double
    , _sumSquaredDivergence :: Double
    , _numExamples          :: Int
    }

-- | A vector of examples.
type Examples = V.Vector PB.Example

-- | A vector of decision trees.
type Trees = V.Vector DecisionTree

informationGain :: Examples -> V.Vector Double
informationGain examples = V.zipWith gain forwards backwards
  where
    forwards = incrementalLoss examples
    backwards = incrementalLoss (V.reverse examples)
    totalLoss = V.last $ incrementalLoss examples
    gain l r = (_sumSquaredDivergence totalLoss -
                _sumSquaredDivergence l +
                _sumSquaredDivergence r) / fromIntegral (V.length examples)
    incrementalLoss =
        V.scanl addExample LossState { _averageLabel=0
                                     , _sumSquaredDivergence=0
                                     , _numExamples=0
                                     }

-- Convenience accessors for label and feature from Protobuf generated
-- code to
label' :: PB.Example -> Double
label' e = fromJust $ PB.label e

features' :: PB.Example -> V.Vector Double
features' example = V.generate (S.length features) (S.index features)
  where
    features = PB.features example

asPBTree' :: DecisionTree -> PB.TreeNode
asPBTree' (Leaf value) = defaultValue { PB.leafValue = Just value }
asPBTree' (Branch f v l r) =
    defaultValue { PB.feature = Just $ fromIntegral f
                 , PB.splitValue = Just  v
                 , PB.left = Just $ asPBTree' l
                 , PB.right = Just $ asPBTree' r
                 }

fromPBTree' :: PB.TreeNode -> DecisionTree
fromPBTree' (PB.TreeNode feature splitValue left right leafValue _)
    | isJust leafValue = Leaf $ fromJust leafValue
    | otherwise = Branch { _feature=(fromIntegral . fromJust) feature
                         , _value=fromJust splitValue
                         , _left=(fromPBTree' . fromJust) left
                         , _right=(fromPBTree' . fromJust) right
                         }

addExample :: LossState -> PB.Example -> LossState
addExample state example =
    LossState { _numExamples=_numExamples state + 1
              , _averageLabel=newAverageLabel
              , _sumSquaredDivergence=newSumSquaredDivergence
              }
  where
    newAverageLabel =
        _averageLabel state + delta / fromIntegral (_numExamples state)
    delta = label' example - _averageLabel state
    newDelta = label' example - newAverageLabel
    newSumSquaredDivergence = _sumSquaredDivergence state +  delta * newDelta

sortFeature :: Int -> Examples -> Examples
sortFeature feature =
    V.fromList .
    sortBy (\l r -> compare (features' l ! feature) (features' r ! feature)) .
    V.toList

findBestSplit :: Examples -> Int -> Split
findBestSplit examples feature =
    Split { _splitFeature=feature
          , _splitValue=features' (samples ! splitPoint) ! feature
          , _averageGain=V.maximum informationGains
          }
  where
    samples = sortFeature feature examples
    informationGains = informationGain samples
    splitPoint = V.maxIndex informationGains

type Features = V.Vector Int

-- TODO(tulloch) - make this more intelligent (support subsampling
-- features for random forests, etc)
getFeatures :: Examples -> Features
getFeatures examples = V.generate numFeatures id
  where
    numFeatures = (V.length . features' . V.head) examples

-- Determines whether a candidate set of splits should happen
shouldSplit :: PB.SplittingConstraints -> Int -> Examples -> Split -> Bool
shouldSplit constraint currentLevel currentExamples candidateSplit =
    and constraints
  where
    constraints = [
     case PB.maximumLevels constraint of
       Nothing -> True
       Just maxLevels -> fromIntegral currentLevel < maxLevels,
     case PB.minimumAverageGain constraint of
       Nothing -> True
       Just minGain -> minGain < _averageGain candidateSplit,
     case PB.minimumSamplesAtLeaf constraint of
       Nothing -> True
       Just minSamples -> minSamples < (fromIntegral . V.length) currentExamples]

buildTreeAtLevel :: (Examples -> Double)
                 -> PB.SplittingConstraints
                 -> Int
                 -> Features
                 -> Examples
                 -> DecisionTree
buildTreeAtLevel leafWeight splittingConstraints level features examples =
    if shouldSplit splittingConstraints level examples bestSplit
    then Branch { _feature=_splitFeature bestSplit
                , _value=_splitValue bestSplit
                , _left=recur leftExamples
                , _right=recur rightExamples
                }
    else Leaf {_value=leafWeight examples} where
        -- candidate splits
        candidates =
            V.map (findBestSplit examples) features
        -- best candidate from all the features
        bestSplit =
            V.maximumBy (compare `on` _averageGain) candidates
        -- sort the examples at this branch by the best feature
        orderedExamples =
            sortFeature (_splitFeature bestSplit) examples
        -- left branch takes <, right branch takes >
        branchLeft ex =
            (features' ex ! _splitFeature bestSplit) <
               _splitValue bestSplit
        -- construct the next level of the tree
        recur =
            buildTreeAtLevel leafWeight splittingConstraints (level + 1) features
        leftExamples = V.takeWhile branchLeft orderedExamples
        rightExamples = V.dropWhile branchLeft orderedExamples

buildTree :: (Examples -> Double)
          -> PB.SplittingConstraints
          -> Features
          -> Examples
          -> DecisionTree
buildTree leafWeight splittingConstraints =
    buildTreeAtLevel leafWeight splittingConstraints 0

predict' :: DecisionTree -> V.Vector Double -> Double
predict' (Leaf v) _ = v
predict' (Branch f v l r) featureVector =
    if featureVector ! f < v
    then predict' l featureVector
    else predict' r featureVector

predictForest' :: Trees -> V.Vector Double -> Double
predictForest' trees featureVector =
    (V.sum . V.map (`predict'` featureVector)) trees

-- | Computes the average activation for each leaf node.
predictForest :: V.Vector PB.TreeNode -> V.Vector Double -> Double
predictForest trees = predictForest' (V.map fromPBTree' trees)

-- | A record that is modified for various loss function
-- implementations.
data LossFunction = LossFunction {
      prior  :: Examples -> Double,
      leaf   :: Examples -> Double,
      weight :: Trees -> PB.Example -> Double
    }

-- | logitLoss is a loss function that implements Algorithm 5 in
-- <http://www-stat.stanford.edu/~jhf/ftp/trebst.pdf>
logitLoss :: LossFunction
logitLoss = LossFunction logitPrior logitLeaf logitWeight
    where
      logitWeight trees example = numerator / denominator
          where
            numerator = 2 * label' example
            denominator = 1 + exp (2 * label' example * prediction)
            prediction = predictForest' trees (features' example)
      logitLeaf examples = numerator / denominator
          where
            numerator = (V.sum . V.map label') examples
            denominator = (V.sum . V.map influence) examples
            influence e = abs (label' e) * (2 - abs (label' e))
      logitPrior examples =  0.5 * log ((1 + averageLabel) / (1 - averageLabel))
          where
            averageLabel = (V.sum . V.map label') examples / fromIntegral (V.length examples)

runBoostingRound :: LossFunction
                 -> PB.SplittingConstraints
                 -> Features
                 -> Examples
                 -> Trees
                 -> DecisionTree
runBoostingRound lossFunction splittingConstraints features examples forest =
    buildTree (leaf lossFunction) splittingConstraints features weightedExamples
  where
    weightedExamples = V.map (\e -> e {PB.label=Just $ weightedLabel e}) examples
    weightedLabel = weight lossFunction forest

-- | Trains a boosted decision tree with the given parameters
trainBoosting
  :: (Enum b, Num b) =>
     LossFunction
     -> b
     -> PB.SplittingConstraints
     -> V.Vector PB.Example
     -> V.Vector PB.TreeNode
trainBoosting lossFunction numRounds splittingConstraints examples =
    V.map asPBTree' trees
  where
    features = getFeatures examples
    trees =
        V.foldl addTree (priorTree examples) (V.enumFromTo 1 numRounds)
    priorTree = V.singleton . Leaf . prior lossFunction
    addTree currentForest _ = V.snoc currentForest weakLearner
      where
        weakLearner = runBoostingRound lossFunction
                                       splittingConstraints
                                       features
                                       examples
                                       currentForest

data RandomForestConfig = RandomForestConfig {
      _numRounds       :: Int,
      _exampleFraction :: Double,
      _featureFraction :: Double
    }

withVector :: Monad m => ([a] -> m [b]) -> V.Vector a -> m (V.Vector b)
withVector f xs = liftM V.fromList (f (V.toList xs))

sampleWithReplacement :: MonadRandom m => Int -> V.Vector a -> m (V.Vector a)
sampleWithReplacement n = withVector (replicateM n . uniform) -- liftM V.fromList $ replicateM n (uniform (V.toList xs))

sampleWithoutReplacement :: (MonadRandom m) => Int -> V.Vector a -> m (V.Vector a)
sampleWithoutReplacement n = withVector (liftM (take n) . shuffleM)

proportion :: (Integral b, RealFrac a) => a -> V.Vector a1 -> b
proportion p xs = ceiling $ (fromIntegral . V.length) xs * p

-- | Trains a random forest with the given constraints.
trainRandomForest
  :: MonadRandom m =>
     RandomForestConfig
     -> PB.SplittingConstraints
     -> V.Vector PB.Example
     -> m (V.Vector PB.TreeNode)
trainRandomForest RandomForestConfig{..} splittingConstraints examples =
      V.mapM addSample (V.enumFromTo 1 _numRounds)
    where
      allFeatures = getFeatures examples
      numSubsampledExamples = proportion _exampleFraction examples
      numSubsampledFeatures = proportion _featureFraction allFeatures
      averageLabel e = (V.sum . V.map label') e / fromIntegral (V.length e)
      weakLearner = buildTree averageLabel splittingConstraints
      addSample _ = do
        candidateExamples <- sampleWithReplacement numSubsampledExamples examples
        candidateFeatures <- sampleWithoutReplacement numSubsampledFeatures allFeatures
        return $ asPBTree' $ weakLearner candidateFeatures candidateExamples
