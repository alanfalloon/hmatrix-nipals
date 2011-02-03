-- | Nonlinear Iterative Partial Least Squares
module Numeric.LinearAlgebra.NIPALS
    ( -- * Simplified Interface
      firstPC
    ) where

import Numeric.Container

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
firstPC :: Product t => Matrix t -> (Vector t, Vector t, Matrix t)
firstPC = undefined

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
