module Main where


import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import System.FilePath


import Data.Boolean.DIMACS
import Data.Boolean.DPLL
import Data.Boolean.SimpleSAT

import qualified Data.IntMap as M
import Data.List
import Data.Ord


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
                  let  p :: Prop
                       p = fromCNF b
                       e = (solve . newSolver ) p
                  print (stat b)
                  putStrLn $ f ++ " solved: " ++ show (not $ null e)



stat :: [[Int]] -> [(Int, (Int, Int, Int))]
stat b = sortBy (comparing val) $ map trans l
    where l = M.toList . foldl' ins M.empty . concat $ map l' b
          l' c = map (\v -> (length c, v)) c
          ins m (l,i) = M.insertWith add2 (abs i) (one l i) m
          add2 (a,b)(c,d) = (a+c,b+d)
          one l i | i> 0      = (l,0)
                  | otherwise = (0,l)
          trans (v,(p,n)) = (v,(p, n, (p*n)))
          val (_, (_,_,l)) = l