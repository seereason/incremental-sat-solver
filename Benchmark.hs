-- |
-- Module      : Main
-- Copyright   : Holger Siegel
-- License     : BSD3
-- 
-- Simple Benchmark for testing SAT problems in DIMACS format.
-- 
-- see http://www.cs.ubc.ca/~hoos/SATLIB/benchm.htm

module Main where


import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import System.FilePath


import Data.Boolean.Dimacs
import Data.Boolean.DPLL
import Data.Boolean.SatSolver
import Data.Boolean.SimpleSAT

import qualified Data.IntMap as M
import Data.List
import Data.Ord


main :: IO ()
main = getArgs >>= getPaths >>= mapM_ solveFile


getPaths :: [String] -> IO [FilePath]
getPaths [p, n]  = fmap (take $ read n) $ getPaths [p]
getPaths [p]     = fmap (map (combine p) . filter isCnf) $ getDirectoryContents p
    where isCnf f = takeExtensions f == ".cnf"
getPaths _       = error "Usage: benchmark CNFDIR [count]"


solveFile :: FilePath -> IO ()
solveFile f = do  b <- fmap parseDimacs (readFile f)
                  let p :: SimpleProp
                      p = fromDimacs b
                      e = (solve . newSolver) p
                  -- print (stat $ clauses b)
                  putStrLn $ f ++ " solved: " ++ show (not $ null e)


-- |
-- Count positive and negative uses of every literal.
-- Returns sums of clause sizes for positve and< negative uses.
-- 
stat :: [[Int]] -> [(Int, (Int, Int))]
stat cnf = sortBy (comparing val) l
    where l = M.toList . foldl' ins M.empty . concat $ map l' cnf
          l' c = map (\v -> (length c, v)) c
          ins m (c,i) = M.insertWith add2 (abs i) (one c i) m
          add2 (a,b)(c,d) = (a+c,b+d)
          one c i | i> 0      = (c,0)
                  | otherwise = (0,c)
          val (_, (p,n)) = - p*n