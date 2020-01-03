module TestData where

import Types
import Data.List

-- Some useful predefined variables
a, b, c, d, e, f, g :: Formula
a = Var "a"
b = Var "b"
c = Var "c"
d = Var "d"
e = Var "e"
f = Var "f"
g = Var "g"

-- Simple test formulae
f1, f2, f3, f4, f5, f6, f7, f8, f9 :: Formula
f1 = Not (Not a)
f2 = Not (Not (Not (Not a)))
f3 = a `And` b
f4 = Not (Not a `And` Not b) 
f5 = And (Not a) (b `Or` c)
f6 = Not (b `Or` (a `And` (Not c)))
f7 = (a `Or` (Not b)) `Or` (a `And` b)
f8 = (a `And` (Not (b `Or` c))) `Or` (Not d)
f9 = Not a `And` ((Not b `And` c) `Or` (Not c `And` d))

-- CNF forms of the above
c1, c2, c3, c4, c5, c6, c7, c8, c9 :: CNF
c1 = a
c2 = a
c3 = a `And` b
c4 = a `Or` b
c5 = (Not a) `And` (b `Or` c)
c6 = (Not b) `And` (Not a `Or` c)
c7 = (a `Or` Not b `Or` a) `And` ((a `Or` Not b) `Or` b)
c8 = (a `Or` Not d) `And` ((Not b `Or` Not d) `And` (Not c `Or` Not d))
c9 = (Not a `And` (((Not b `Or` Not c) `And` (c `Or` Not c)) `And` 
                   ((Not b `Or` d) `And` (c `Or` d)))) 

-- Some additional CNFs and their flattened representations, for testing
-- propagation and dpll
cnf0 :: CNF
cnf0 
  = a `And` a

cnf0Rep
  = [[1],
     [1]]

cnf1 :: CNF
cnf1
  = foldr1 And ors
  where
    ors = [foldr1 Or [a, b, c],
           foldr1 Or [a, b, Not c],
           foldr1 Or [a, Not b, c],
           foldr1 Or [a, Not b, Not c],
           foldr1 Or [Not a, b, c],
           foldr1 Or [Not a, b, Not c],
           foldr1 Or [Not a, Not b, c]]

cnf1Rep :: CNFRep
cnf1Rep 
  = [[1, 2, 3],
     [1, 2, -3],
     [1, -2, 3],
     [1, -2, -3],
     [-1, 2, 3],
     [-1, 2, -3],
     [-1, -2, 3]]

cnf2 :: CNF
cnf2 
  = foldr1 And ors
  where
    ors = [foldr1 Or [Not a, Not d],
           foldr1 Or [b, a, e],
           foldr1 Or [c, Not b],
           d,
           foldr1 Or [c, Not e],
           foldr1 Or [Not c, Not g],
           foldr1 Or [a, g, f],
           foldr1 Or [Not c, f]]

cnf2Rep :: CNFRep
cnf2Rep
  = [[-1, -4],
     [2, 1, 5],
     [3, -2],
     [4],
     [3, -5],
     [-3, -7],
     [1, 7, 6],
     [-3, 6]]

cnf3 :: CNF
cnf3 
  = foldr1 And ors
  where
    ors = [foldr1 Or [Not c, Not g],
           foldr1 Or [b, c, e],
           foldr1 Or [a, Not b],
           foldr1 Or [a, Not e],
           foldr1 Or [Not a, Not d],
           foldr1 Or [c, d, f],
           foldr1 Or [Not a, Not f],
           g]

cnf3Rep :: CNFRep
cnf3Rep
  = [[-3, -7],
     [2, 3, 5],
     [1, -2],
     [1, -5],
     [-1, -4],
     [3, 4, 6],
     [-1, -6],
     [7]]

-- This is a variant of the $n$-queens problem below, but with rooks instead
-- of queens. This example is for a 2x2 board. There are two solutions.
cnf4 :: CNF
cnf4
  = foldr1 And ors
  where
    ors = [foldr1 Or [Not a, Not b],
           foldr1 Or [Not a, Not c],
           foldr1 Or [Not b, Not d],
           foldr1 Or [Not c, Not d],
           a `Or` b,
           c `Or` d]
           
cnf4Rep :: CNFRep
cnf4Rep
  = [[-1, -2],
     [-1, -3],
     [-2, -4],
     [-3, -4],
     [1, 2],
     [3, 4]]

--------------------------------------------------------------------------

-- Generates constraints for the n-queens problem as a propositional 
-- formula in CNF
queensCNF :: Int -> CNF
queensCNF n
  = foldr1 And (concatMap (\f -> f n) [cols, rows, diag1, diag1', diag2, diag2'] ++ 
                qcol n)
  where
    pos :: Int -> Int -> Int -> Formula
    pos n i j 
      = Var ("x" ++ show ((i - 1) * n + j))

    cols :: Int -> [CNF]
    cols n
      = [(Not (pos n i j)) `Or` (Not (pos n i k)) | 
          i <- [1..n], j <- [1..n], k <- [j+1..n]]

    rows :: Int -> [CNF]
    rows n
      = [(Not (pos n j i)) `Or` (Not (pos n k i)) | 
          i <- [1..n], j <- [1..n], k <- [j+1..n]]

    diag1 :: Int -> [CNF]
    diag1 n
      = [(Not (pos n (d + j) j)) `Or` (Not (pos n (d + k) k)) |
          d <- [0..n-2], j <- [1..n-d], k <- [j+1..n-d]]

    diag1' :: Int -> [CNF]
    diag1' n
      = [(Not (pos n j (j - d))) `Or` (Not (pos n k (k - d))) |
          d <- [negate (n-2)..(-1)], j <- [1..n+d], k <- [j+1..n+d]]

    diag2 :: Int -> [CNF]
    diag2 n
      = [(Not (pos n j (d - j))) `Or` (Not (pos n k (d - k))) |
          d <- [3..n+1], j <- [1..d-1], k <- [j+1..d-1]]

    diag2' :: Int -> [CNF]
    diag2' n
      = [(Not (pos n j (d - j))) `Or` (Not (pos n k (d - k))) |
          d <- [(n+2)..2*n-1], j <- [d-n..n], k <- [j+1..n]]

    qcol :: Int -> [CNF]
    qcol n
      = [foldr1 Or [pos n i j | j <- [1..n]] | i <- [1..n]]

-- As above, but generating the flattened representation directly.
-- Note: it is not in general the case that flatten (queensCNF n) ==
-- queensCNFRep n because of the variable numbering convention use here.
queensCNFRep :: Int -> CNFRep
queensCNFRep n
  = concatMap (\f -> f n) [cols, rows, diag1, diag1', diag2, diag2', qcol]
  where
    pos :: Int -> Int -> Int -> Int
    pos n i j 
      = (i - 1) * n + j

    cols :: Int -> CNFRep
    cols n
      = [[negate (pos n i j), negate (pos n i k)] | 
          i <- [1..n], j <- [1..n], k <- [j+1..n]]

    rows :: Int -> CNFRep
    rows n
      = [[negate (pos n j i), negate (pos n k i)] | 
          i <- [1..n], j <- [1..n], k <- [j+1..n]]

    diag1 :: Int -> CNFRep
    diag1 n
      = [[negate (pos n (d + j) j), negate (pos n (d + k) k)] |
          d <- [0..n-2], j <- [1..n-d], k <- [j+1..n-d]]

    diag1' :: Int -> CNFRep
    diag1' n
      = [[negate (pos n j (j - d)), negate (pos n k (k - d))] |
          d <- [negate (n-2)..(-1)], j <- [1..n+d], k <- [j+1..n+d]]

    diag2 :: Int -> CNFRep
    diag2 n
      = [[negate (pos n j (d - j)), negate (pos n k (d - k))] |
          d <- [3..n+1], j <- [1..d-1], k <- [j+1..d-1]]

    diag2' :: Int -> CNFRep
    diag2' n
      = [[negate (pos n j (d - j)), negate (pos n k (d - k))] |
          d <- [(n+2)..2*n-1], j <- [d-n..n], k <- [j+1..n]]

    qcol :: Int -> CNFRep
    qcol n
      = [[pos n i j | j <- [1..n]] | i <- [1..n]]

-- Displays one board on the assumption that the variables are numbered in 
-- increasing order from top left to bottom right, e.g. 1 to 64 for 8-queens.
printBoard :: [Int] -> Int -> IO()
printBoard 
  = show . sortBy (\x y -> compare (abs x) (abs y)) 
  where
    show [] _
      = putStrLn ""
    show sol n
      = do
          putStrLn (toString row)
          show rows n
      where
        (row, rows) = splitAt n sol
        toString = concatMap (\v -> if v < 0 then "O " else "X ") 

-- Optional extra...
printAllBoards :: [[Int]] -> Int -> IO()
printAllBoards sol n
  = mapM_ (flip printBoard n) sol

