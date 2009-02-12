-- |
-- Module      : Data.Boolean.DPLL
-- Copyright   : Holger Siegel
-- License     : BSD3
-- 
module Data.Boolean.DPLL (

  Solvable, fromProposition, fromDimacs, conjunction, nextLiteral, freeze,
  isSatisfied, 
  ) where

import Data.Boolean.Proposition(Proposition)
import Data.Boolean.Dimacs(Dimacs)


class Solvable a where
    fromProposition  :: Proposition -> a
    fromDimacs       :: Dimacs -> a
    conjunction      :: a -> a -> a

    -- |
    -- Finds the next literal to be specialized.
    -- If the result is positve, then the non-negated literal
    -- will be tried with higher priority by the solver.
    -- Otherwise the negated literal will be preferred.
    -- 
    -- The first component of the result is the number of times
    -- the literal occurs in the current clause. The second component
    -- is the varibale index of the literal.
    --
    -- Returns @(0, 0)@ if there is no clause to be satisfied.
    nextLiteral  :: a -> (Int, Int)

    -- |
    -- @freeze n b@ returns a copy of @b@ in which every literal with index 
    -- @n@ is set to @True@ and every literal with index @(-n)@ is set to @False@.
    freeze     :: Int -> a -> a

    -- | 
    -- This monadic action checks whether the @SatSolver@ has found a solution.
    -- If all constraints are solved, then it returns @True@.
    -- If there are unsolved constraints, then it returns @False@.
    -- It @fail@s when the given constraints are known to be unsatisfiable.
    isSatisfied  :: Monad m => a -> m Bool

