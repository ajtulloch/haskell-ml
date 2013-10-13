module MachineLearning.Common where

import Data.Vector as V

data Example = Example {
      features :: V.Vector Double
    , label :: Double
    }
