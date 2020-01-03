module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp a table
  = fromJust (lookup a table)

-- 3 marks
vars :: Formula -> [Id]
vars 
  = sort . nub . vars' 
  where 
    vars' :: Formula -> [Id]
    vars' f 
      | Var id    <- f = [id]
      | Not f'    <- f = vars f' 
      | And f1 f2 <- f = vars f1 ++ vars f2 
      | Or f1 f2  <- f = vars f1 ++ vars f2

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF f
  | Not (Or f1 f2)  <- f = And (toNNF (Not f1)) (toNNF (Not f2))
  | Not (And f1 f2) <- f = Or (toNNF (Not f1)) (toNNF (Not f2))
  | Not (Not f')    <- f = toNNF f'
  | And f1 f2       <- f = And (toNNF f1) (toNNF f2)
  | Or f1 f2        <- f = Or (toNNF f1) (toNNF f2)
  | otherwise            = f 

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' (toNNF f)
  where    
    toCNF' :: NNF -> CNF 
    toCNF' f 
      | Or f1 f2  <- f = distribute (toCNF' f1) (toCNF' f2)
      | And f1 f2 <- f = And (toCNF' f1) (toCNF' f2)
      | otherwise      = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  -- | And (And f1 f1') (And f2 f2') <- f = flatten (And f1 f1') ++ flatten (And f2 f2')
  | And f1           (And f2 f2') <- f = flatten' f1 : flatten (And f2 f2')
  -- | And (And f1 f1') f2           <- f = flatten' f2 : flatten (And f1 f1')
  | And f1           f2           <- f = flatten' f1 : flatten' f2 : []
  | otherwise                          = flatten' f : []

  where 
    ids = idMap f 

    flatten' :: CNF -> [Int]
    -- No And structures can exist at this level, since formula is in CNF
    flatten' f 
      -- | Or (Or f1 f1') (Or f2 f2') <- f = flatten' (Or f1 f1') ++ flatten' (Or f2 f2')
      | Or f1          (Or f2 f2') <- f = (flatten'' f1) : (flatten' (Or f2 f2'))
      -- | Or (Or f1 f1') f2          <- f = (flatten'' f2) : (flatten' (Or f1 f1'))
      | Or f1          f2          <- f = (flatten'' f1) : (flatten'' f2) : []
      | otherwise                       = flatten'' f : []

    flatten'' :: CNF -> Int 
    flatten'' f 
      | Not (Var id) <- f = - (lookUp id ids)
      | Var id       <- f = lookUp id ids

-- And (Or (Var "a") (Not (Var "d"))) (And (Or (Not (Var "b")) (Not (Var "d"))) (Or (Not (Var "c")) (Not (Var "d"))))

-- And (Or (Not (Var "c")) (Not (Var "g"))) (And (Or (Var "b") (Or (Var "c") (Var "e"))) 
-- (And (Or (Var "a") (Not (Var "b"))) (And (Or (Var "a") (Not (Var "e"))) (And (Or (Not (Var "a"))
--  (Not (Var "d"))) (And (Or (Var "c") (Or (Var "d") (Var "f"))) (And (Or (Not (Var "a"))
--   (Not (Var "f"))) (Var "g")))))))


-- Var Id
--              | Not Formula
--              | And Formula Formula
--              | Or  Formula Formula


--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits f
  | [] <- units = (f, [])
  | otherwise   = (f'', concat units ++ units')
  -- = undefined --(deleteLiterals (f \\ (units)), concat units)
  where 
    units        = findUnits f
    (f', units') = propUnits (foldl deleteLiterals (f \\ units) (concat units))
    f''          = f' \\ [[]]

    findUnits :: CNFRep -> [[Int]]
    findUnits f
      | [n] : clauses <- f = [n] : (findUnits clauses)
      | _ : clauses   <- f = findUnits clauses
      | otherwise          = []

    deleteLiterals :: CNFRep -> Int -> CNFRep
    deleteLiterals f n 
      | (clause : clauses) <- f = ((clause \\ [n]) \\ [-n]) : (deleteLiterals clauses n)
      | otherwise               = []


-- 4 marks
-- dp :: CNFRep -> [[Int]]
-- dp f
--   | [] <- cnf1, [] <- cnf2 = [propagatedUnits ++ units1] ++ [propagatedUnits ++ units2]
--   | [] <- cnf1 = [propagatedUnits ++ units1] 
--   | [] <- cnf2 = [propagatedUnits ++ units2]
--   | otherwise  = []
--   -- = undefined --propUnits ([firstLiteral] : propagatedCNF)
--   where
--     ( propagatedCNF@((firstLiteral:_):_), propagatedUnits) = propUnits f
--     (cnf1, units1) = propUnits ([firstLiteral] : propagatedCNF)
--     (cnf2, units2) = propUnits ([-firstLiteral] : propagatedCNF)


dp :: CNFRep -> [[Int]]
dp []
  = []
dp f
  | [] <- propagatedCNF = [propagatedUnits]
  | otherwise           = ( propagatedUnits : (dp ([firstLiteral] : propagatedCNF)) ) ++ (propagatedUnits : (dp ([-firstLiteral] : propagatedCNF)))
  -- = undefined --propUnits ([firstLiteral] : propagatedCNF)
  where
    (propagatedCNF, propagatedUnits) = propUnits f
    ((firstLiteral:_):_) = propagatedCNF 
    -- (cnf1, units1) = propUnits ([firstLiteral] : propagatedCNF)
    -- (cnf2, units2) = propUnits ([-firstLiteral] : propagatedCNF)


    -- acc param w solutions


--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


