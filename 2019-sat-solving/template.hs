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
flatten formula
  = flatten' formula
  where 
    ids = idMap formula
    flatten' :: CNF -> CNFRep
    flatten' f
      | Var x <- f       = [[lookUp x ids]] 
      | Not (Var x) <- f = [[-1 * (lookUp x ids)]] 
      | And f1 f2   <- f = flatten' f1 ++ flatten' f2
      | Or f1 f2    <- f = [concat (flatten' f1 ++ flatten' f2)] 

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits f
  | [] <- units = (f, [])
  | otherwise   = (f', unit : units')
  where 
    units        = findUnits f
    (unit:_)     = units
    (f', units') = propUnits (deleteLiterals (deleteClauses f unit) unit)

    deleteClauses :: CNFRep -> Int -> CNFRep
    deleteClauses f n 
      = filter (not . elem n) f

    findUnits :: CNFRep -> [Int]
    findUnits f
      | [n] : clauses <- f = n : (findUnits clauses)
      | _ : clauses   <- f = findUnits clauses
      | otherwise          = []

    deleteLiterals :: CNFRep -> Int -> CNFRep
    deleteLiterals f n 
      | (clause : clauses) <- f = (clause \\ [-n]) : (deleteLiterals clauses n)
      | otherwise               = []


dp :: CNFRep -> [[Int]]
dp f
  = dp' f []
  where
    dp' :: CNFRep -> [Int] -> [[Int]]
    dp' f assignments
      | [] <- propagatedCNF   = [assignments ++ propagatedUnits]
      | elem [] propagatedCNF = [] -- failure
      | otherwise             = dp' ([firstLiteral] : propagatedCNF) (propagatedUnits ++ assignments)
                                ++ dp' ([-firstLiteral] : propagatedCNF) (propagatedUnits ++ assignments)
      where 
        (propagatedCNF, propagatedUnits)  = propUnits f
        ((firstLiteral:_):_)              = propagatedCNF 


--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f
  = map toVerbose (allAssignments sols)
  where 
    idsFlipped = flipTable (idMap f)
    sols = dp (flatten (toCNF f))
    varsNumerical = map (flip lookUp (idMap f)) (vars f) 

    toVerbose :: [Int] -> [(Id, Bool)]
    toVerbose sols 
      = sort (map g sols)
      where 
        g :: Int -> (Id, Bool)
        g n = (lookUp (abs n) idsFlipped, n > 0) 

    allAssignments :: [[Int]] -> [[Int]]
    allAssignments []
      = [] 
    allAssignments (a:as)
      = (map ((++) a) (combinations (findMissing varsNumerical a))) ++ (allAssignments as)

    combinations :: [Int] -> [[Int]]
    combinations vs
      | [] <- vs = [[]] 
      | (var:vars) <- vs = (map ((:) var) (combinations vars)) ++ (map ((:) (-var)) (combinations vars))

    findMissing :: [Int] -> [Int] -> [Int]
    findMissing allVars assignments
      = filter (not . flip elem (map abs assignments) ) allVars

    flipTable :: [(a,b)] -> [(b,a)]
    flipTable t
      | []            <- t = [] 
      | ((a, b) : ps) <- t = (b, a) : (flipTable ps)

 