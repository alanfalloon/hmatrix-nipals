{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Nonlinear Iterative Partial Least Squares
module Numeric.LinearAlgebra.NIPALS
    ( -- * Simplified Interface
      firstPC
    , firstPCFromScores
      -- * Monadic interface
    , firstPCFromScoresM
    , residual
    ) where

import Data.List (foldl1')
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
                    in dscores
      (t,p) = let steps' = zip convergence $ tail steps
                  steps'' = dropWhile (\(c,_) -> c > threshold) steps'
              in snd $ head steps''
      r = m `sub` (t `outer` p)

      refine (t,_) = (t', p')
          where
            p' = toUnit (trans m <> t)
            t' = m <> p'

      threshold = sqrt (fromIntegral (cols m + rows m)) * eps

diffScores :: Vector Double -> Vector Double -> Double
diffScores ta tb = let xa = ta<.>ta
                       xb = tb<.>tb
                       x = abs $ xa - xb
                   in (x / (eps+xb)) :: Double

toUnit :: Vector Double -> Vector Double
toUnit v = if mag <= 0.0 then dim v |> (1 : repeat 0) else scale (1/mag) v
    where
      mag = norm2 v

-- | Calculate the first principal component -- calculating the
-- samples fresh on every pass.
--
-- This function calculates the exact same results as
-- 'firstPCFromScores' (minus the residual), but instead of an input
-- 'Matrix', it takes a monad action that yields the list of samples,
-- and it guarantees that the list returned by the action will be
-- consumed in a single pass. However the action may be demanded many
-- times.
--
-- The residual can't be calculated lazily, like it is in
-- 'firstPCFromScores', because the samples would need to be
-- demanded. Instead, to calculate the residual use 'residual'.
--
-- There is no corresponding @firstPCM@ that guesses the initial score
-- vector for you because if you need to use this function instead of
-- 'firstPC', then you really should come up with a reasonable
-- starting point or it will take forever.
firstPCFromScoresM :: Monad m =>
                      m [Vector Double]
                   -> Vector Double
                   -> m (Vector Double, Vector Double)
firstPCFromScoresM samplesM t0 = do
  (t,p) <- refine t0
  iter t0 t p
      where
        refine t = do
          p <- samplesM >>= (\s -> return $ transMult1Pass s t)
          let p' = toUnit p
          t' <- samplesM >>= (\s -> return $ mult1Pass s p')
          return (t',p')
        threshold = sqrt (fromIntegral (2 * dim t0)) * eps
        iter t0 t1 p1 | diff < threshold = return $ (p1,t1)
                      | otherwise = do
                                   (t2,p2) <- refine t1
                                   iter t1 t2 p2
                     where
                       diff = diffScores t0 t1

-- Single pass versions of (trans m <> v) and (m <> v) respectively,
-- where m is a list of rows
transMult1Pass, mult1Pass :: [Vector Double] -> Vector Double -> Vector Double
mult1Pass rows vec = fromList $ map (dot vec) rows
transMult1Pass cols vec = foldl1' add $ zipWith scale (toList vec) cols

-- | Calculate the residuals of a series of samples given a component
-- and score vector.
--
-- > (p,t) <- firstPCFromScoresM samplesM (randomVector 0 Gaussian numSamples)
-- > samples <- samplesM
-- > let r = residual samples p t
residual :: [Vector Double] -- ^ The samples
         -> Vector Double   -- ^ The component (also called the loading)
         -> Vector Double   -- ^ The scores
         -> [Vector Double] -- ^ The residuals for each sample
residual s p t = r
    where
      ts = toList t
      comp = map (`scale` p) ts
      r = zipWith sub s comp
