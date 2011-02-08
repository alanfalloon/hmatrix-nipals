{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Nonlinear Iterative Partial Least Squares
module Numeric.LinearAlgebra.NIPALS
    ( -- * Simplified Interface
      firstPC
    , firstPCFromScores
    ) where

import Numeric.LinearAlgebra

-- | Calculate the first principal component of a set of samples.
--
-- Each row in the matrix is one sample. Note that this is transposed
-- compared to the implementation of principal components using 'svd'
-- or 'leftSV'
--
-- Example:
-- 
-- > let (pc,scores,residuals) = firstPC $ fromRows samples
-- 
-- This is calculated by providing a default estimate of the scores to
-- 'firstPCFromScores'
firstPC :: Matrix Double -> (Vector Double, Vector Double, Matrix Double)
firstPC m = firstPCFromScores m t0
    where
      t0 = head $ toColumns m

-- | Calculate the first principal component of a set of samples given
-- a starting estimate of the scores.
--
-- Each row in the matrix is one sample. Note that this is transposed
-- compared to the implementation of principal components using 'svd'
-- or 'leftSV'
--
-- The second argument is a starting guess for the score vector. If
-- this is close to the actual score vector, then this will cause the
-- algorthm to converge much faster.
--
-- Example:
-- 
-- > let (pc,scores,residuals) = firstPCFromScores (fromRows samples) scoresGuess
-- 
firstPCFromScores :: Matrix Double
                  -> Vector Double
                  -> (Vector Double, Vector Double, Matrix Double)
firstPCFromScores m t0 = (p,t,r)
    where
      steps = iterate refine (t0,undefined)
      convergence = let scores = map fst steps
                        dscores = zipWith diffScores scores $ tail scores
                    in smooth dscores
      (t,p) = let steps' = zip convergence $ tail steps
                  steps'' = dropWhile (\(c,_) -> c > threshold) steps'
              in snd $ head steps''
      r = m `sub` (t `outer` p)

      refine (t,_) = (t', p')
          where
            p' = toUnit (trans m <> t)
            t' = m <> p'

      diffScores ta tb = let xa = ta<.>ta
                             xb = tb<.>tb
                             x = abs $ xa - xb
                         in (x / xb) :: Double

      threshold = sqrt (fromIntegral (cols m + rows m)) * eps

toUnit :: Vector Double -> Vector Double
toUnit v = if mag <= 0.0 then dim v |> (1 : repeat 0) else scale (1/mag) v
    where
      mag = norm2 v

smooth :: [Double] -> [Double]
smooth (x0:xs@(x1:_)) = (2*x0+x1)/3 : smooth xs
smooth _ = []

-- | Calculate the first principal component -- calculating the
-- samples fresh on every pass.
--
-- This function calculates the exact same results as 'firstPC', but
-- instead of an input 'Matrix', it takes a monad action that yields
-- the list of samples, and it guarantees that the list returned by
-- the action will be consumed in a single pass. However the action
-- may be demanded many times.
firstPCM :: (Product t, Monad m) => m [Vector t] -> m (Vector t, Vector t, [Vector t])
firstPCM = undefined
