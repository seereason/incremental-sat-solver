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
-- The implementation uses the basic DPLL algorithm with unit propagation.
-- 
module Data.Boolean.DPLL (

  Solvable, fromProposition, fromCNF, conjunction, nextLiteral, specWith,

  SatSolver, valuation,

  newSolver,
  isSatisfied, 

  lookupVar,
  assertTrue,
  branch,
  branchOnVar,
  selectBranchVar,
  solve
  ) where

import qualified Data.IntMap as IM
import Control.Monad(MonadPlus, mplus)

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


    specWith     :: Int -> a -> a

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
      dpllFormula :: a }
               

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
assertTrue :: Solvable a => Proposition -> SatSolver a -> SatSolver a
assertTrue b s = s { dpllFormula = conjunction expr constraint }
    where expr = dpllFormula s
          constraint = fromProposition (specialize lv b)
          lv n = fmap fromBool $ lookupVar n s
          fromBool True   = Yes
          fromBool False  = No


-- |
-- This function guesses a value for the next variable.
-- As this is a non-deterministic operation, the
-- resulting solvers are returned in an instance of @MonadPlus@.
-- 
branch ::  (MonadPlus m, Solvable a) => SatSolver a -> m (SatSolver a)
branch s
    | c == 0     = return s
    | c == 1     = simplify v s
    | otherwise  = simplify2 v s
    where (c,v) = nextLiteral (dpllFormula s)

-- |
-- This function guesses a value for the given variable, if it is
-- currently unbound. As this is a non-deterministic operation, the
-- resulting solvers are returned in an instance of @MonadPlus@.
-- 
branchOnVar ::  (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
branchOnVar v s 
    | IM.member v (valuation s)
        = return s
    | otherwise
        = simplify2 v s

-- |
-- We select a variable from the shortest clause hoping to produce a
-- unit clause.
--
selectBranchVar :: Solvable a => SatSolver a -> Maybe Int
selectBranchVar s = case nextLiteral (dpllFormula s) of
                      (0,0) -> Nothing
                      (_,v) -> Just v

-- |
-- solves ...
solve :: (MonadPlus m, Solvable a) => SatSolver a -> m (IM.IntMap Bool)
solve s = case isSatisfied (dpllFormula s) of
            Just True  -> return (valuation s)
            Just False  -> fail "no solution"
            Nothing    -> branch s >>= solve



simplify :: (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
simplify v' = return . freezeVar v'

simplify2 :: (MonadPlus m, Solvable a) => Int -> SatSolver a -> m (SatSolver a)
simplify2 v s =  let guess1 = simplify v s
                     guess2 = simplify (-v) s
                 in guess1 `mplus` guess2



freezeVar :: Solvable a => Int -> SatSolver a -> SatSolver a
freezeVar n (SatSolver val x)
    | n >= 0     = s' n True
    | otherwise  = s' (-n) False
    where s' v b = SatSolver { valuation = IM.insert v b val,
                               dpllFormula  = specWith n x }



