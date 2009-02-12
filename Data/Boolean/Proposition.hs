{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- |
-- Module      : Data.Boolean.Proposition
-- Copyright   : Holger Siegel
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This library provides a representation of boolean formulas that is
-- used by the solver in "Data.Boolean.SatSolver".
-- 
module Data.Boolean.Proposition ( 
    Proposition(..),
    specialize, mkVar
  ) where

import Data.Maybe (  fromMaybe )



-- |
-- Boolean formulas are represented as values of type @Proposition@.
-- 
data Proposition
  -- | Variables are labeled with a positive @Int@,
  = Var Int
  -- | @Yes@ represents /true/,
  | Yes
  -- | @No@ represents /false/,
  | No
  -- | @Not@ constructs negated formulas,
  | Not Proposition
  -- | and finally we provide conjunction,
  | Proposition :&&: Proposition
  -- | disjunction of boolean formulas.
  | Proposition :||: Proposition
  -- | and equality of boolean formulas.
  | Proposition :==: Proposition
 deriving Show


-- |
-- This function creates a literal from a literal index.
-- A positive value n is transformed into a |Var n|.
-- A negative value (-n) is transformed into a the expresison |Not (Var n)|.
-- 
mkVar :: Int -> Proposition
mkVar n
    | n > 0 = Var n
    | n < 0 = Not (Var (-n))
    | otherwise = error "mkVar: illegal variable index 0"


-- |
-- Optionally replaces literals by propositions
specialize :: (Int -> Maybe Proposition) -> Proposition -> Proposition
specialize a = spec
    where spec (Not x) = Not (spec x)
          spec (x :&&: y) = spec x :&&: spec y
          spec (x :||: y) = spec x :||: spec y
          spec x@(Var v)  = fromMaybe x (a v)
          spec x          = x
