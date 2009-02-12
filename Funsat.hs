-- |
-- Module      : Main
-- Copyright   : Holger Siegel
-- License     : BSD3
-- 
-- Simple Benchmark for testing SAT problems in DIMACS format.
-- 
-- see http://www.cs.ubc.ca/~hoos/SATLIB/benchm.htm
--
-- It uses the funsat library written by Denis Bueno. 

module Main where


import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import System.FilePath

import Funsat.Types
import Funsat.Solver

import qualified Data.Set as S

main :: IO ()
main = do -- [path] <- getArgs
  files <- getArgs >>= getPaths
  -- let path = "ex/uf20-0195.cnf"
  mapM_ solveFile files


getPaths :: [String] -> IO [FilePath]
getPaths [p, n]  = fmap (take $ read n) $ getPaths [p]
getPaths [p]     = fmap (map (combine p) . filter isCnf) $ getDirectoryContents p
    where isCnf f = takeExtensions f == ".cnf"
getPaths _       = error "Usage: benchmark CNFDIR [count]"

solveFile :: FilePath -> IO ()
solveFile f = do  b <- fmap fromDIMACS (readFile f)
                  let (_, _, t) = solve1 b
                  let solved Nothing = True 
                      solved _       = False
                  putStrLn $ f ++ " solved: " ++ show (solved t)


fromDIMACS :: String -> CNF
fromDIMACS s = let ls'' = clauses
               in CNF nv nc (S.fromList ls'')
    where ls          = map words $ filter noComment $ lines s
          (nv, nc)  = getHeader (head ls)
          clauses     = filter (not . null) $  map parseClause $ take nc $ tail ls

          getHeader ["p", "cnf", v, c] = (read v::Int, read c :: Int)
          getHeader _ = error "no DIMACS header"

          noComment [] = True
          noComment ('c':_) = False
          noComment _ = True

          parseClause = map L . takeWhile (/= 0) . map read

