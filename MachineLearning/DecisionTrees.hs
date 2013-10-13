module MachineLearning.DecisionTrees where

import MachineLearning.Common

import Data.Vector as V
import qualified Data.List
import Data.Function

data DecisionTree = Leaf {
      value :: Double
    } | Branch {
      feature :: Int
    , value :: Double
    , left :: DecisionTree
    , right :: DecisionTree
    }

data Split = Split {
      splitFeature :: Int
    , splitValue :: Double
    , averageGain :: Double
    }

data LossState = LossState {
      averageLabel :: Double
    , sumSquaredDivergence :: Double
    , numExamples :: Int
    }

informationGain :: Vector Example -> Vector Double
informationGain examples =
    V.zipWith gain (incrementalLoss examples) (incrementalLoss (V.reverse examples)) where
        totalLoss = V.last $ incrementalLoss examples
        gain l r = 
            (sumSquaredDivergence totalLoss - 
             sumSquaredDivergence l + 
             sumSquaredDivergence r) / fromIntegral (V.length examples)
        incrementalLoss = V.scanl addExample LossState{
                            averageLabel=0
                          , sumSquaredDivergence=0
                          , numExamples=0
                          }

addExample :: LossState -> Example -> LossState
addExample state example = LossState {
                             numExamples=numExamples state + 1
                           , averageLabel=newAverageLabel
                           , sumSquaredDivergence=newSumSquaredDivergence
                           } where
    newAverageLabel = averageLabel state + delta / fromIntegral (numExamples state)
    delta = label example - averageLabel state
    newDelta = label example - newAverageLabel
    newSumSquaredDivergence = sumSquaredDivergence state + delta * newDelta

removeExample :: LossState -> Example -> LossState
removeExample state example = LossState {
                                numExamples=numExamples state - 1
                              , averageLabel=newAverageLabel
                              , sumSquaredDivergence=newSumSquaredDivergence
                              } where
    newAverageLabel = averageLabel state - delta / fromIntegral (numExamples state)
    delta = label example - averageLabel state
    newDelta = label example - newAverageLabel
    newSumSquaredDivergence = sumSquaredDivergence state - delta * newDelta

sortFeature :: Vector Example -> Int -> Vector Example
sortFeature examples feature =
  V.fromList
       (Data.List.sortBy
            (\l r -> compare (features l ! feature) (features r ! feature))
            (V.toList examples))

findBestSplit :: Vector Example -> Int -> Split
findBestSplit examples feature = Split {
                                   splitFeature=feature
                                 , splitValue=features (samples ! splitPoint) ! feature
                                 , averageGain=V.maximum informationGains
                              } where
    samples = sortFeature examples feature
    informationGains = informationGain samples
    splitPoint = maxIndex informationGains

-- TODO(tulloch) - make this more intelligent (support subsampling
-- features for random forests, etc)
getFeatures :: Vector Example -> Vector Int
getFeatures examples = V.fromList [0..numFeatures] where
    numFeatures = V.length (features $ V.head examples)

data SplittingConstraint = SplittingConstraint {
      maximumLevels :: Maybe Int
    , minimumAverageGain :: Maybe Double
    , minimumSamplesAtLeaf :: Maybe Int
    }

-- Determines whether a candidate set of splits should happen
shouldSplit :: SplittingConstraint -> Int -> Vector Example -> Split -> Bool
shouldSplit constraint currentLevel currentExamples candidateSplit = 
    Data.List.and constraints where
        constraints = [
         case maximumLevels constraint of
                     Nothing -> True
                     Just maxLevels -> currentLevel < maxLevels,
         case minimumAverageGain constraint of
                     Nothing -> True
                     Just minGain -> minGain < averageGain candidateSplit,
         case minimumSamplesAtLeaf constraint of
                     Nothing -> True
                     Just minSamples -> minSamples < V.length currentExamples]
    
buildTreeAtLevel leafWeight splittingConstraint level examples = 
    if shouldSplit splittingConstraint level examples bestSplit
    then Branch {
               feature=splitFeature bestSplit
             , value=splitValue bestSplit
             , left=recur $ V.takeWhile takePredicate orderedExamples
             , right=recur $ V.dropWhile takePredicate orderedExamples
             }
    else Leaf (leafWeight examples) where
        -- candidate splits
        candidates = V.map (findBestSplit examples) (getFeatures examples)
        -- best candidate from all the features
        bestSplit = V.maximumBy (compare `on` averageGain) candidates
        -- sort the examples at this branch by the best feature
        orderedExamples = sortFeature examples (splitFeature bestSplit)
        -- left branch takes <, right branch takes >
        takePredicate ex = features ex ! splitFeature bestSplit < splitValue bestSplit
        -- construct the next level of the tree
        recur = buildTreeAtLevel leafWeight splittingConstraint (level + 1)

buildTree leafWeight splittingConstraint = buildTreeAtLevel leafWeight splittingConstraint 0

predict :: DecisionTree -> Vector Double -> Double
predict (Leaf v) _ = v
predict (Branch f v l r) featureVector = 
    if featureVector ! f < v then predict l featureVector else predict r featureVector 

predictForest :: Vector DecisionTree -> Vector Double -> Double
predictForest trees featureVector = V.sum (V.map (`predict` featureVector) trees)

-- Typeclass for a given loss function
class LossFunction a where
    prior :: a -> Vector Example -> Double
    leaf :: a -> Vector Example -> Double
    weight :: a -> Vector DecisionTree -> Example -> Double

data LogitLoss = LogitLoss deriving (Show, Eq)

-- From Algorithm 5 in http://www-stat.stanford.edu/~jhf/ftp/trebst.pdf
instance LossFunction LogitLoss where 
   prior _ examples = 0.5 * log ((1 + averageLabel) / (1 - averageLabel)) where
    averageLabel = V.sum (V.map label examples) / fromIntegral (V.length examples)
     
   leaf _ examples = numerator / denominator where
     numerator = V.sum (V.map label examples)

     denominator = V.sum (V.map (\e -> abs (label e) * (2 - abs (label e))) examples) 
   
   weight _ trees example = (2 * label example) / 
                            (1 + exp (2 * label example * predictForest trees (features example)))

runBoostingRound :: (LossFunction a) => a -> SplittingConstraint -> Vector Example -> Vector DecisionTree -> DecisionTree
runBoostingRound lossFunction splittingConstraint examples forest =
    buildTree (leaf lossFunction) splittingConstraint weightedExamples where
        weightedExamples = V.map (\e -> e {label=weightedLabel e}) examples
        weightedLabel = weight lossFunction forest
                           
boosting :: (LossFunction a) => a -> Int -> SplittingConstraint -> Vector Example -> Vector DecisionTree
boosting lossFunction numRounds splittingConstraint examples =
   V.foldl addTree (V.singleton priorTree) (V.replicate numRounds 0) where
        priorTree = Leaf (prior lossFunction examples)
        addTree currentForest _ = V.snoc currentForest weakLearner where
          weakLearner = runBoostingRound lossFunction splittingConstraint examples currentForest
