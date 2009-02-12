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

  Proposition(..), SatSolver,

  newSatSolver, isSolved, 

  lookupVar, assertTrue, branchOnVar, selectBranchVar, solve, isSolvable, branch

  ) where

import Data.Boolean.DPLL
import Data.Boolean.Proposition

import qualified Data.IntMap as IM
import Control.Monad(MonadPlus, mplus)

-- | A new SAT solver without stored constraints.
-- 
newSatSolver :: Solvable a => SatSolver a
newSatSolver = SatSolver IM.empty (fromProposition Yes)

-- | This predicate tells whether all constraints are solved.
-- 
isSolved :: Solvable a => SatSolver a -> Bool
isSolved a = isSatisfied (formula a) == Just True


-- | 
-- We can assert boolean formulas to update a @SatSolver@. The
-- assertion may fail if the resulting constraints are unsatisfiable.
-- 
assertTrue :: (MonadPlus m, Solvable a) => Proposition -> SatSolver a -> m (SatSolver a)
assertTrue f s
    | failed     = fail "impossible"
    | otherwise  = return s'
    where  failed = isSatisfied (formula s) == Just False
           s' = addConstraint f s

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
selectBranchVar s = case nextLiteral (formula s) of
                      (0,0) -> Nothing
                      (_,v) -> Just v

-- | 
-- This function guesses values for variables such that the stored
-- constraints are satisfied. The result may be non-deterministic and
-- is, hence, returned in an instance of @MonadPlus@.
-- 
solve :: (MonadPlus m, Solvable a) => SatSolver a -> m (IM.IntMap Bool)
solve s = case isSatisfied (formula s) of
            Just True  -> return (valuation s)
            Just False  -> fail "no solution"
            Nothing    -> branch s >>= solve


-- |
-- This predicate tells whether the stored constraints are
-- solvable. Use with care! This might be an inefficient operation. It
-- tries to find a solution using backtracking and returns @True@ if
-- and only if that fails.
-- 
isSolvable :: Solvable a => SatSolver a -> Bool
isSolvable = not . null . solve


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
    where (c,v) = nextLiteral (formula s)


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
                               formula  = freeze n x }

