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
      [ testProperty "sameFirstPCAsSVD" prop_sameFirstPCAsSVD
      , testProperty "scoreGuessStable" prop_scoreGuessStable
      ]
    , testGroup "Correctness"
      [ testProperty "resultCanRecoverInput" prop_recoverInput
      ]
    ]


prop_sameFirstPCAsSVD m = relErr <= svdThreshold
    where
      relErrP = relativeDifference expPC actPC
      relErrN = relativeDifference expPC (-actPC)
      relErr = min relErrP relErrN
      expPC = head $ toColumns lSV
      (lSV,_) = leftSV $ trans m
      (actPC,_,_) = firstPC m

      svdThreshold = 10 * sqrt (fromIntegral (rows m * cols m) * eps)

prop_recoverInput m = relErr <= eps
    where
      (p,t,r) = firstPC m
      m' = (t `outer` p) `add` r
      relErr = relativeDifference m m'

prop_scoreGuessStable m = tRelErr <= th .&&. pRelErr <= th
    where
      (p,t,_) = firstPC m
      (p',t',_) = firstPCFromScores m t
      tRelErr = relativeDifference t t'
      pRelErr = relativeDifference p p'
      th = sqrt $ fromIntegral (cols m * rows m) * eps

relativeDifference :: (Normed c t, Container c t) => c t -> c t -> Double
relativeDifference x y = realToFrac (norm (x `sub` y) / (peps + norm x + norm y))
    where norm = pnorm Infinity


instance (Arbitrary t, Storable t) => Arbitrary (Matrix t) where
    arbitrary = sized $ \n -> do
                  r <- choose (1,n+1)
                  c <- choose (1,n+1)
                  vals <- vector (r*c)
                  return $ (r><c) vals
