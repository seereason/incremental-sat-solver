module Data.Boolean.DIMACS(fromDIMACS) where


-- |
-- Parse a DIMACS file. /Quick and dirty: Will probably crash on wrong input./
fromDIMACS :: String -> [[Int]]
fromDIMACS s = clauses
    where ls          = map words $ filter noComment $ lines s
          numClauses  = getHeader (head ls)
          clauses     = filter (not . null) $  map parseClause $ take numClauses $ tail ls

          getHeader ["p", "cnf", _, n] = read n :: Int
          getHeader _ = error "no DIMACS header"

          noComment [] = True
          noComment ('c':_) = False
          noComment _ = True

          parseClause =  takeWhile (/= 0) . map read
                            
