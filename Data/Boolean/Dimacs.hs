module Data.Boolean.Dimacs(Dimacs(..), parseDimacs) where


data Dimacs = Dimacs {
      numLiterals  :: Int,
      numClauses   :: Int,
      clauses      :: [[Int]]
    }


-- |
-- Parse a DIMACS file. /Quick and dirty: Will probably crash on wrong input./
parseDimacs :: String -> Dimacs
parseDimacs s = Dimacs nl nc cls
    where ls          = map words $ filter noComment $ lines s
          (nl, nc)    = getHeader (head ls)
          cls         = filter (not . null) $  map parseClause $ take nc $ tail ls

          getHeader ["p", "cnf", l, c] = (read l, read c) :: (Int, Int)
          getHeader _ = error "no DIMACS header"

          noComment [] = True
          noComment ('c':_) = False
          noComment _ = True

          parseClause =  takeWhile (/= 0) . map read
                            
