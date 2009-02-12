-- |
-- Module      : Data.Boolean.DPLL
-- Copyright   : Holger Siegel
-- License     : BSD3
-- 
module Data.Boolean.DPLL (

  Solvable, fromProposition, fromCNF, conjunction, nextLiteral, freeze,

  SatSolver(..),

  newSolver,
  isSatisfied, 

  lookupVar,
  addConstraint
  ) where

import qualified Data.IntMap as IM


import Data.Boolean.Proposition


class Solvable a where
    fromProposition  :: Proposition -> a
    fromCNF          :: [[Int]] -> a
    conjunction      :: a -> a -> a

    -- |
    -- Finds the smallest clause and returns its size together with a 
    -- literal that occurs in this clause. Returns @(0,0)@ if there
    -- is no clause to be satisfied.
    nextLiteral  :: a -> (Int, Int)


    freeze     :: Int -> a -> a

    -- | 
    -- This predicate tells whether the @SatSolver@ has found a solution.
    -- Returns @Just True@ if all constraints are solved, and @Just False@
    -- if the given formula is not satisfyable. Returns @Nothing@ if the
    -- formula is still unsolved.
    isSatisfied  :: a -> Maybe Bool


-- | 
-- A @SatSolver@ can be used to solve boolean formulas.
-- It contains the Boolean formula to be solved and
-- and a valuation that assigns logical values to literals.
data SatSolver a = SatSolver {
      -- | Returns the @SatSolver@'s current valuation as a mapping from
      --   variable indices to Boolean values
      valuation   :: IM.IntMap Bool, 
      formula :: a }
               

-- |
-- Returns a new @SatSolver@ that contains the
-- given formula together with the empty valuation
--
newSolver :: Solvable a => a -> SatSolver a
newSolver = SatSolver IM.empty




-- |
-- We can lookup the binding of a variable according to the currently
-- stored constraints. If the variable is unbound, the result is
-- @Nothing@.
-- 
lookupVar :: Int -> SatSolver a -> Maybe Bool
lookupVar name = IM.lookup name . valuation


-- | 
-- We can assert boolean formulas to update a @SatSolver@. The
-- assertion may fail if the resulting constraints are unsatisfiable.
-- 
addConstraint :: Solvable a => Proposition -> SatSolver a -> SatSolver a
addConstraint b s = s { formula = conjunction expr constraint }
    where expr = formula s
          constraint = fromProposition (specialize lv b)
          lv n = fmap fromBool $ lookupVar n s
          fromBool True   = Yes
          fromBool False  = No



