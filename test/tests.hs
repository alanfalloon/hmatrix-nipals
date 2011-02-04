import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.NIPALS
import Foreign.Storable
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((><))

main = defaultMain tests

tests =
    [ testGroup "Accuracy"
      [ testProperty "sameAsSVD" prop_sameAsSVD
      ]
    ]


prop_sameAsSVD m = diff <= eps'
    where
      eps' = eps * fromIntegral (rows m * cols m)
      diff = norm2 $ flatten $ expPC - actPC
      expPC :: Matrix Double
      (expPC,_) = leftSV m
      actPC = fromColumns actPClist
      actPClist = take (rows m) $ unfoldr getPCs m
      getPCs m' = let (pc,_,m'') = firstPC m'
                  in Just (pc,m'')

instance (Arbitrary t, Storable t) => Arbitrary (Matrix t) where
    arbitrary = sized $ \n -> do
                  r <- choose (1,n+1)
                  c <- choose (1,n+1)
                  vals <- vector (r*c)
                  return $ (r><c) vals
