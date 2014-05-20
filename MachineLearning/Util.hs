module MachineLearning.Util where

import qualified Data.Matrix     as M
import qualified Data.Vector     as V
import           Test.QuickCheck

instance (Arbitrary a) =>  Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

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

differenceSymmetric :: (Floating a, Eq a) => V.Vector a -> V.Vector a -> Bool
differenceSymmetric = symmetric difference

-- innerProduct m returns the mapping x -> x^T M x
innerProduct :: (Num a) => M.Matrix a -> V.Vector a -> a
innerProduct m v = (M.transpose cv * m * cv) M.! (1, 1)
  where
    cv = M.colVector v
