module Solver where

import Data.List
import Data.Char

import Types
import WordData
import Clues
import Examples

------------------------------------------------------
-- Part I

notNull :: [a] -> Bool
notNull = not . null

punctuation :: String
punctuation 
  = "';.,-!?"

cleanUp :: String -> String
cleanUp 
  = map toLower . filter (not . (`elem` punctuation))

split2 :: [a] -> [([a], [a])]
split2 xs = [splitAt i xs | i <- [1..(length xs - 1)]]

split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(xs1', xs1'', xs2) | (xs1, xs2) <- split2 xs, (xs1', xs1'') <- split2 xs1] ++ 
    [(xs1, [], xs2) | (xs1, xs2) <- split2 xs, length xs1 == 1 || length xs2 == 1]

uninsert :: [a] -> [([a], [a])]
uninsert xs
  = [(s, f ++ t) | (f, s, t) <- split3 xs, notNull s]

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs] 
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs

------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches s (Synonym s')        = s `elem` synonyms s'
matches s (Anagram i s')      = sort s == sort s'
matches s (Reversal i t)      = (reverse s) `matches` t
matches s (Insertion i t1 t2) = or [s1 `matches` t1 && s2 `matches` t2 | (s1, s2) <- uninsert s]
matches s (Charade i t1 t2)   = or [s1 `matches` t1 && s2 `matches` t2 | (s1, s2) <- split2M s]
matches _ _                   = False


evaluate :: Parse -> Int -> [String]
evaluate (d, _, t) l
  = filter (\s -> s `matches` t && length s == l) (synonyms (unwords d))

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym ws
  | notNull $ synonyms (unwords ws) 
    = [Synonym (unwords ws)]
  | otherwise
    = []

parseAnagram :: [String] -> [ParseTree]
parseAnagram ws
  = [Anagram [unwords s1] (concat s2)| (s1, s2) <- split2M ws, (unwords s1) `elem` anagramIndicators]

parseReversal :: [String] -> [ParseTree]
parseReversal ws
  = [Reversal [unwords s1] t | (s1, s2) <- split2M ws, (unwords s1) `elem` reversalIndicators, t <- parseWordplay s2]

parseInsertion :: [String] -> [ParseTree]
parseInsertion w
  = l1 ++ l2 
  where
    l1 = [Insertion [unwords ws] t1 t2 | (arg, ws, arg') <- split3 w, (unwords ws) `elem` insertionIndicators, t1 <- parseWordplay arg, t2 <- parseWordplay arg']
    l2 = [Insertion [unwords ws] t2 t1 | (arg, ws, arg') <- split3 w, (unwords ws) `elem` envelopeIndicators, t1 <- parseWordplay arg, t2 <- parseWordplay arg']

parseCharade :: [String] -> [ParseTree]
parseCharade w
  = l1 ++ l2 ++ l3
  where
    l1 = [Charade [] t1 t2 | (arg, ws, arg') <- split3 w, unwords ws == [], t1 <- parseWordplay arg, t2 <- parseWordplay arg']
    l2 = [Charade [unwords ws] t1 t2 | (arg, ws, arg') <- split3 w, (unwords ws) `elem` (beforeIndicators \\ [[]]), t1 <- parseWordplay arg, t2 <- parseWordplay arg']
    l3 = [Charade [unwords ws] t2 t1 | (arg, ws, arg') <- split3 w, (unwords ws) `elem` afterIndicators, t1 <- parseWordplay arg, t2 <- parseWordplay arg']

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText ws
  = [(d, l, wordplay) | (d, l, wp) <- splt, unwords l `elem` linkWords, (notNull . synonyms) (unwords d), wordplay <- parseWordplay wp]
  where
    splt = split3M ws

solve :: Clue -> [Solution]
solve clue@(s, n) 
  = [(clue, parse, head $ evaluate parse n) | parse <- parses, notNull $ evaluate parse n] 
  where
    parses = parseClue clue


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]


