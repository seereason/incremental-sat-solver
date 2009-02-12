module Data.Boolean.SimpleSAT(Prop, (.&&.), (.||.)) where

import Data.Boolean.Proposition
import Data.Boolean.DPLL




data Term
  = VAR !Int
  | AND Term Term
  | OR Term Term

-- |
-- Propositions represented as Boolean formulas. (Hopefully) more efficient than
-- transforming everything to Conjunctive Normal Form.
-- 
-- /WARNING: The Boolean operator/ @:==:@ /is not supported yet./
data Prop = Y | N
          | V !Int !Int !Int Term

infixr 2  .||.
infixr 3  .&&.

(.&&.) :: Prop -> Prop -> Prop
Y .&&. b  = b
N .&&. _  = N
a .&&. Y  = a
_ .&&. N  = N
V ca da va ba .&&. V cb db vb bb
     | ca <= cb  = V ca (da+db) va (AND ba bb)
     | otherwise = V cb (da+db) vb (AND bb ba)

(.||.) :: Prop -> Prop -> Prop
Y  .||. _  = Y
N  .||. b  = b
_  .||. Y  = Y
a  .||. N  = a
V ca da va ba .||. V cb db vb bb
  | da <= db  = V (ca+cb) da va (OR ba bb)
  | otherwise = V (ca+cb) db vb (OR bb ba)



instance Solvable Prop where

    fromProposition = pos
        where
          neg (Var n)    = let a = -n
                           in V 1 1 a (VAR a)
          neg Yes        = N
          neg No         = Y
          neg (Not x)    = pos x
          neg (x :&&: y) = neg x .||. neg y
          neg (x :||: y) = neg x .&&. neg y
      
          pos (Var n)    = V 1 1 n (VAR n)
          pos Yes        = Y
          pos No         = N
          pos (Not x)    = neg x
          pos (x :&&: y) = pos x .&&. pos y
          pos (x :||: y) = pos x .||. pos y

    fromCNF []   = Y
    fromCNF xs   = foldr (\ c b -> fromClause c .&&. b) Y xs
        where fromClause = foldr (\ v b -> V 1 1 v (VAR v) .||. b) N
    
    conjunction = (.&&.)

    nextLiteral (V c _ v _) = (c, v)
    nextLiteral _ = (0, 0)

    freeze _ Y = Y
    freeze _ N = N
    freeze v (V _ _ _ x) = f x
        where  f a@(VAR v')
                   | v == v'  = Y
                   | -v == v' = N
                   | otherwise = V 1 1 v' a
               f (AND a b) = f a .&&. f b
               f (OR a b)  = f a .||. f b   

    isSatisfied Y = Just True
    isSatisfied N = Just False
    isSatisfied _ = Nothing


