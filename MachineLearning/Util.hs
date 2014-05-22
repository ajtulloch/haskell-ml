module MachineLearning.Util where

import qualified Data.Matrix                       as M
import           Data.Maybe
import qualified Data.Sequence                     as S
import qualified Data.Vector                       as V
import qualified MachineLearning.Protobufs.Example as PB
import           Test.QuickCheck

newtype ArbVec a = ArbVec { unVec :: V.Vector a } deriving (Show)

instance Arbitrary a => Arbitrary (ArbVec a) where
    arbitrary = fmap (ArbVec . V.fromList) arbitrary

label' :: PB.Example -> Double
label' e = fromJust $ PB.label e

features' :: PB.Example -> V.Vector Double
features' example = V.generate (S.length features) (S.index features)
  where
    features = PB.features example

norm :: Floating a => V.Vector a -> a
norm x = sqrt $ dotProduct x x

difference :: Floating a => V.Vector a -> V.Vector a -> a
difference x y = norm (V.zipWith (-) x y)


dotProduct :: Num b => V.Vector b -> V.Vector b -> b
dotProduct x y = V.foldl (+) 0 (V.zipWith (*) x y)

-- QuickCheck properties
normPositive :: (Floating a, Ord a) => V.Vector a -> Bool
normPositive x = norm x >= 0

symmetric :: Eq a => (t -> t -> a) -> t -> t -> Bool
symmetric f x y = f y x == f x y

dotProductSymmmetric  :: (Eq a, Num a) => V.Vector a -> V.Vector a -> Bool
dotProductSymmmetric = symmetric dotProduct

differenceSymmetric :: (Floating a, Eq a) => ArbVec a -> ArbVec a -> Bool
differenceSymmetric l r = symmetric difference (unVec l) (unVec r)

-- innerProduct m returns the mapping x -> x^T M x
innerProduct :: (Num a) => M.Matrix a -> V.Vector a -> a
innerProduct m v = let cv = M.colVector v in (M.transpose cv * m * cv) M.! (1, 1)
