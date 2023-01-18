module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x xs = foldl' (\acc v -> if v == x then acc + 1 else acc) 0 xs

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es) = [(n, count n e1s + count n e2s) | n <- ns]
  where
    (e1s, e2s) = unzip es

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es) = [e1 | (e1, e2) <- es, e2 == n] ++ [e2 | (e1, e2) <- es, e1 == n]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode a (ns, es) = (filter (/= a) ns, filter (\(e1, e2) -> e1 /= a && e2 /= a) es)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _) = []
colourGraph c g@(nodes, edges)
  = (n, getColour cMap n g) : cMap
  where
    ((n, _) : _) = sortOn snd $ degrees g
    g'           = removeNode n g
    cMap         = colourGraph c g'

    getColour :: (Eq a, Show a) => Colouring a -> a -> Graph a -> Colour
    getColour []        n' g'' = 1
    getColour colouring n' g'' = getColour' assignedColours 1
      where
        connectedNodes  = neighbours n' g''
        assignedColours = sort $ map (flip lookUp colouring) connectedNodes

        getColour' :: [Int] -> Int -> Int
        getColour' list current
          | elem current list = getColour' list (current + 1)
          | otherwise         = if current > c then 0 else current

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap cs = ("return", "return") : map transform cs
  where
    transform :: (Id, Colour) -> (Id, Id)
    transform (id, 0) = (id, id)
    transform (id, n) = (id, "R" ++ show n)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments ids idMap = map (\id -> Assign (lookUp id idMap) (Var id)) ids

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var id)         idMap = Var (lookUp id idMap)
renameExp (Apply op e1 e2) idMap = Apply op (renameExp e1 idMap) (renameExp e2 idMap)
renameExp e                idMap = e

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b idMap = filter (not . selfAssignments) $ map renameStatement b
  where
    renameStatement :: Statement -> Statement
    renameStatement (Assign id e) 
      = Assign (lookUp id idMap) (renameExp e idMap) 
    renameStatement (If e tb fb)  
      = If (renameExp e idMap) (renameBlock tb idMap) (renameBlock fb idMap)
    renameStatement (While e b)
      = While (renameExp e idMap) (renameBlock b idMap)

    selfAssignments :: Statement -> Bool
    selfAssignments (Assign id (Var id')) = id == id'
    selfAssignments _                     = False

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = foldl (\(n, e) lv -> (nub $ lv ++ n, nub $ perms lv ++ e)) ([], [])
  where
    perms :: [a] -> [Edge a]
    perms []       = []
    perms [x]      = []
    perms (x : xs) = [(x, x') | x' <- xs] ++ perms xs

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars cfg = converge (==) $ iterate (flip go [0..(length cfg - 1)]) (replicate (length cfg) [])
  where
    def  = map (fst . fst) cfg
    use  = map (snd . fst) cfg
    succ = map snd cfg

    go :: [[Id]] -> [Int] -> [[Id]]
    go _ []       
      = []
    go l (n : ns) 
      = (union (use !! n) ((foldl union [] (map ((!!) l) (succ !! n))) \\ [def !! n])) : go l ns

    converge :: Eq a => (a -> a -> Bool) -> [a] -> a
    converge f (x : xs : xss)
      | f x xs   = x
      | otherwise = converge f (xs : xss)

buildCFG :: Function -> CFG
buildCFG (_, _, block)
  = buildCFG' block 0
  where
    buildCFG' :: Block -> Int -> CFG
    buildCFG' [] _
      = []
    buildCFG' ((Assign "return" e) : _) n = [(("return", getVars e), [])]
    buildCFG' ((Assign id exp) : block) n 
      = ((id, getVars exp), [n + 1]) : buildCFG' block (n + 1)
    buildCFG' ((If e tb fb) : block) n
      = (("_", getVars e), [n + 1, n + numLines' tb + 1]) : 
        (buildCFG' tb (n + 1) ++ buildCFG' fb (n + numLines' tb + 1) ++ buildCFG' block (n + numLines' tb + numLines' fb + 1))
    buildCFG' ((While e b) : block) n
      = (("_", getVars e), [n + 1, n + numLines' b + 1]) : 
        (buildCFG' (init b) (n + 1) ++ buildCFG' [last b] (n - 1) ++ buildCFG' block (n + numLines' b + 1))

    getVars :: Exp -> [Id]
    getVars (Const _) = []
    getVars (Var id)  = [id]
    getVars (Apply _ e1 e2) = getVars e1 ++ getVars e2
  
    numLines :: Statement -> Int
    numLines (Assign _ _ ) = 1
    numLines (If _ b1 b2)  = 1 + numLines' b1 + numLines' b2
    numLines (While _ b)   = 1 + numLines' b

    numLines' :: Block -> Int
    numLines' = (sum . map numLines)