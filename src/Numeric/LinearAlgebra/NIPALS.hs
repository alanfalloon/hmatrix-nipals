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
-- > let (pc,scores,residual) = firstPC $ fromRows samples
-- 
firstPC :: Product t => Matrix t -> (Vector t, Vector t, Matrix t)
firstPC = undefined

