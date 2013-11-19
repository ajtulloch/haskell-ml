module MachineLearning.Hopfield
    (HopfieldNet(..),
     initialize,
     initializeWith,
     activity,
     train,
     associate,
     energy) where

import           Control.Monad        (foldM)
import qualified Control.Monad.Random as R
import qualified Data.Matrix          as M
import qualified Data.Vector          as V
import           MachineLearning.Util

data HopfieldNet = HopfieldNet { _state   :: V.Vector Float
                               , _weights :: M.Matrix Float
                               } deriving (Show)

activity :: Float -> Float
activity activation = if activation <= 0 then -1.0 else 1.0

activityProp :: Float -> Bool
activityProp x = x == 0 || activity x == signum x

initialize :: Int -> HopfieldNet
initialize n = HopfieldNet (V.replicate n 0) (M.zero n n)

initializeWith :: M.Matrix Float -> HopfieldNet
initializeWith patterns = train state patterns
  where
    state = initialize (M.ncols patterns)

update' :: HopfieldNet -> Int -> HopfieldNet
update' (HopfieldNet state weights) neuron = HopfieldNet newState weights
  where
    newState = state V.// [(neuron, activity activation)]
    -- Vector is indexed from 0, Matrix is indexed from 1.
    activation = dotProduct (M.getCol (neuron + 1) weights) state

update :: R.MonadRandom m => HopfieldNet -> m HopfieldNet
update current =  do
  i <-  R.getRandomR (0, (V.length . _state) current - 1)
  return $ update' current i

train :: HopfieldNet -> M.Matrix Float -> HopfieldNet
train (HopfieldNet state weights) patterns =
    HopfieldNet state (weights + updates)
  where
    updates = M.matrix n n weight
    n = V.length state
    weight (i, j) = 1.0 / (fromIntegral . M.nrows) patterns *
                    dotProduct (M.getCol i patterns) (M.getCol j patterns)

settle :: R.MonadRandom m => HopfieldNet -> Int -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

associate
  :: R.MonadRandom m =>
     HopfieldNet -> Int -> V.Vector Float -> m (V.Vector Float)
associate net iterations pattern =
    do
      settled <-  settle (net { _state = pattern }) iterations
      return $ _state settled

energy :: HopfieldNet -> Float
energy (HopfieldNet state weights) = -0.5 * innerProduct weights state
