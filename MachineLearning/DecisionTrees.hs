module MachineLearning.DecisionTrees
    (LossFunction(..),
     Examples,
     trainBoosting,
     predictForest) where

import           Data.Function                                  (on)
import           Data.List                                      (and, sortBy)
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

type Examples = V.Vector PB.Example
type Trees = V.Vector DecisionTree

informationGain :: Examples -> V.Vector Double
informationGain examples =
    V.zipWith gain (incrementalLoss examples) (incrementalLoss (V.reverse examples)) where
        totalLoss = V.last $ incrementalLoss examples
        gain l r =
            (_sumSquaredDivergence totalLoss -
             _sumSquaredDivergence l +
             _sumSquaredDivergence r) / fromIntegral (V.length examples)
        incrementalLoss = V.scanl addExample LossState{
                            _averageLabel=0
                          , _sumSquaredDivergence=0
                          , _numExamples=0
                          }

-- Convenience accessors for label and feature from Protobuf generated
-- code to
label' :: PB.Example -> Double
label' e = fromJust $ PB.label e

features' :: PB.Example -> V.Vector Double
features' example = V.generate (S.length features) (S.index features) where
    features = PB.features example

asPBTree' :: DecisionTree -> PB.TreeNode
asPBTree' (Leaf value) = defaultValue { PB.leafValue = Just value }
asPBTree' (Branch f v l r) = defaultValue { PB.feature = Just $ fromIntegral f
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
addExample state example = LossState {
                             _numExamples=_numExamples state + 1
                           , _averageLabel=newAverageLabel
                           , _sumSquaredDivergence=newSumSquaredDivergence
                           } where
    newAverageLabel = _averageLabel state + delta / fromIntegral (_numExamples state)
    delta = label' example - _averageLabel state
    newDelta = label' example - newAverageLabel
    newSumSquaredDivergence = _sumSquaredDivergence state + delta * newDelta

sortFeature :: Examples -> Int -> Examples
sortFeature examples feature =
  V.fromList
       (sortBy
            (\l r -> compare (features' l ! feature) (features' r ! feature))
            (V.toList examples))

findBestSplit :: Examples -> Int -> Split
findBestSplit examples feature = Split {
                                   _splitFeature=feature
                                 , _splitValue=features' (samples ! splitPoint) ! feature
                                 , _averageGain=V.maximum informationGains
                              } where
    samples = sortFeature examples feature
    informationGains = informationGain samples
    splitPoint = V.maxIndex informationGains

-- TODO(tulloch) - make this more intelligent (support subsampling
-- features for random forests, etc)
getFeatures :: Examples -> V.Vector Int
getFeatures examples = V.fromList [0..numFeatures] where
    numFeatures = V.length (features' $ V.head examples)

-- Determines whether a candidate set of splits should happen
shouldSplit :: PB.SplittingConstraints -> Int -> Examples -> Split -> Bool
shouldSplit constraint currentLevel currentExamples candidateSplit = and constraints where
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

buildTreeAtLevel  :: (Examples -> Double) -> PB.SplittingConstraints -> Int -> Examples -> DecisionTree
buildTreeAtLevel leafWeight splittingConstraints level examples =
    if shouldSplit splittingConstraints level examples bestSplit
    then Branch {
               _feature=_splitFeature bestSplit
             , _value=_splitValue bestSplit
             , _left=recur $ V.takeWhile takePredicate orderedExamples
             , _right=recur $ V.dropWhile takePredicate orderedExamples
             }
    else Leaf (leafWeight examples) where
        -- candidate splits
        candidates = V.map (findBestSplit examples) (getFeatures examples)
        -- best candidate from all the features
        bestSplit = V.maximumBy (compare `on` _averageGain) candidates
        -- sort the examples at this branch by the best feature
        orderedExamples = sortFeature examples (_splitFeature bestSplit)
        -- left branch takes <, right branch takes >
        takePredicate ex = features' ex ! _splitFeature bestSplit < _splitValue bestSplit
        -- construct the next level of the tree
        recur = buildTreeAtLevel leafWeight splittingConstraints (level + 1)

buildTree :: (Examples -> Double) -> PB.SplittingConstraints -> Examples -> DecisionTree
buildTree leafWeight splittingConstraints = buildTreeAtLevel leafWeight splittingConstraints 0

predict' :: DecisionTree -> V.Vector Double -> Double
predict' (Leaf v) _ = v
predict' (Branch f v l r) featureVector =
    if featureVector ! f < v then predict' l featureVector else predict' r featureVector

predictForest' :: Trees -> V.Vector Double -> Double
predictForest' trees featureVector = V.sum (V.map (`predict'` featureVector) trees)

predictForest :: V.Vector PB.TreeNode -> V.Vector Double -> Double
predictForest trees featureVector = predictForest' (V.map fromPBTree' trees) featureVector

-- Typeclass representing a usable loss function
class LossFunction a where
    -- Prior maps
    prior :: a -> Examples -> Double
    leaf :: a -> Examples -> Double
    weight :: a -> Trees -> PB.Example -> Double

data LogitLoss = LogitLoss deriving (Show, Eq)

-- From Algorithm 5 in http://www-stat.stanford.edu/~jhf/ftp/trebst.pdf
instance LossFunction LogitLoss where
   prior _ examples = 0.5 * log ((1 + averageLabel) / (1 - averageLabel)) where
    averageLabel = V.sum (V.map label' examples) / fromIntegral (V.length examples)

   leaf _ examples = numerator / denominator where
     numerator = V.sum (V.map label' examples)
     denominator = V.sum (V.map (\e -> abs (label' e) * (2 - abs (label' e))) examples)

   weight _ trees example = (2 * label' example) /
                            (1 + exp (2 * label' example * predictForest' trees (features' example)))


runBoostingRound :: LossFunction a => a -> PB.SplittingConstraints -> Examples -> Trees -> DecisionTree
runBoostingRound lossFunction splittingConstraints examples forest =
    buildTree (leaf lossFunction) splittingConstraints weightedExamples where
        weightedExamples = V.map (\e -> e {PB.label=Just (weightedLabel e)}) examples
        weightedLabel = weight lossFunction forest

trainBoosting :: (LossFunction a) => a -> Int -> PB.SplittingConstraints -> Examples -> V.Vector PB.TreeNode
trainBoosting lossFunction numRounds splittingConstraints examples = V.map asPBTree' trees where
    trees = V.foldl addTree (V.singleton priorTree) (V.replicate numRounds (0 :: Int))
    priorTree = Leaf (prior lossFunction examples)
    addTree currentForest _ = V.snoc currentForest weakLearner where
        weakLearner = runBoostingRound lossFunction splittingConstraints examples currentForest
