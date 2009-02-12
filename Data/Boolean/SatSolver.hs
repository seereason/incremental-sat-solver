-- |
-- Module      : Data.Boolean.SatSolver
-- Copyright   : Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an implementation of the
-- Davis-Putnam-Logemann-Loveland algorithm
-- (cf. <http://en.wikipedia.org/wiki/DPLL_algorithm>) for the boolean
-- satisfiability problem. It not only allows to solve boolean
-- formulas in one go but also to add constraints and query bindings
-- of variables incrementally.
-- 
-- The implementation is not sophisticated at all but uses the basic
-- DPLL algorithm with unit propagation.
-- 
module Data.Boolean.SatSolver (

  -- * Propositions and CNFs
  Proposition(..),
  Dimacs(..),

  -- * Creating solvers and propositions
  SatSolver, newSolver, emptySolver,

  fromDimacs, fromProposition,
  assertTrue,

  -- * Querying
  isSolved, 
  lookupVar,
  nextBranchVar,

  -- * Incremental solving
  branch, branchOnVar,
  solve, isSolvable

  ) where


import Data.Boolean.DPLL
import Data.Boolean.Proposition
import Data.Boolean.Dimacs(Dimacs(..))

import qualified Data.IntMap as IM
import Control.Monad(MonadPlus, mplus)


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
-- Returns a new @SatSolver@ that contains the
-- proposition 'Yes' together with the empty valuation
--
emptySolver :: Solvable a => SatSolver a
emptySolver = SatSolver IM.empty (fromProposition Yes)


-- | 
-- We can assert boolean formulas to update a @SatSolver@. The
-- assertion may fail if the resulting constraints are unsatisfiable.
-- 
assertTrue :: (MonadPlus m, Solvable a) => Proposition -> SatSolver a -> m (SatSolver a)
assertTrue f = prune . addConstraint f


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


-- |
-- This predicate tells whether all constraints are solved.
-- 
isSolved :: Solvable a => SatSolver a -> Bool
isSolved a = isSatisfied (formula a) == Just True


-- |
-- We can lookup the binding of a variable according to the currently
-- stored constraints. If the variable is unbound, the result is
-- @Nothing@.
-- 
lookupVar :: Int -> SatSolver a -> Maybe Bool
lookupVar name = IM.lookup name . valuation


-- |
-- We select a variable from the shortest clause hoping to produce a
-- unit clause.
--
nextBranchVar :: Solvable a => SatSolver a -> Maybe Int
nextBranchVar s = case nextLiteral (formula s) of
                      (0,0) -> Nothing
                      (_,v) -> Just v


-- |
-- This function nondeterministically guesses a value for the next variable.
-- The resulting solvers are returned in an instance of @MonadPlus@.
-- 
branch ::  (MonadPlus m, Solvable a) => SatSolver a -> m (SatSolver a)
branch s
    | c == 0     = prune s
    | c == 1     = freezeVar v s
    | otherwise  = branchOnVar' v s
    where (c,v) = nextLiteral (formula s)



-- |
-- This function guesses a value for the given variable, if it is
-- currently unbound. As this is a non-deterministic operation, the
-- resulting solvers are returned in an instance of @MonadPlus@.
--
-- If the variable index is negative, then the negated literal will be
-- tried first.
-- 
branchOnVar ::  (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
branchOnVar v = doBranch
    where  doBranch s
               | IM.member (abs v) (valuation s) = prune s
               | otherwise = branchOnVar' v s


-- | 
-- This function tries to find a valuation that satisfies the given
-- constraints. The result may be non-deterministic and is, hence,
-- returned in an instance of @MonadPlus@.
-- 
solve :: (MonadPlus m, Solvable a) => SatSolver a -> m (SatSolver a)
solve = go 
    where go s  | isSolved s  = return s
                | otherwise   = branch s >>= solve


-- |
-- This predicate tells whether the stored constraints are
-- solvable. Use with care! This might be an inefficient operation. It
-- tries to find a solution using backtracking and returns @True@ if
-- and only if that fails.
-- 
isSolvable :: Solvable a => SatSolver a -> Bool
isSolvable = not . null . solve



{-
  Helper functions
-}
 

prune :: (Solvable a, Monad m) => SatSolver a -> m (SatSolver a)
prune s = isSatisfied (formula s) >> return s


freezeVar :: (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
freezeVar n (SatSolver val x)
    | n >= 0     = go n True
    | otherwise  = go (-n) False
    where go v b = prune SatSolver {
                     valuation = IM.insert v b val,
                     formula  = freeze n x }


branchOnVar' :: (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
branchOnVar' v s =  let guess1 = freezeVar v s
                        guess2 = freezeVar (-v) s
                    in guess1 `mplus` guess2

