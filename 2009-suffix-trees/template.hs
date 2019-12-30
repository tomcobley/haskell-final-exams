data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix _ ""
  = False
isPrefix (a:as) (b:bs)
  = (a == b) && isPrefix as bs

removePrefix :: String -> String -> String
--Pre: s is a prefix of s'
removePrefix "" b
  = b
removePrefix (a:as) (b:bs)
  = removePrefix as bs

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes s
  = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring a b
  = or (map (isPrefix a) (suffixes b))

findSubstrings :: String -> String -> [Int]
findSubstrings a b
  =[ n | n <- [0..(length b - 1)], isPrefix a (suffs !! n) ]
  where
    suffs = suffixes b

------------------------------------------------------
-- Node [("banana",Leaf 0),
-- ("a",Node [("",Leaf 5),("na",Node [("",Leaf 3),("na",Leaf 1)])]),
-- ("na",Node [("",Leaf 4),("na",Leaf 2)])]

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n)
  = [n]
getIndices (Node ts)
  = concatMap (getIndices . snd) ts

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition
  = undefined


-- NOT WORKING FULLY :

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Node [])
  = []
findSubstrings' "" (Leaf n)
  = [n]
-- findSubstrings' (c:cs) (Leaf n)
--   = []
findSubstrings' s ( Node ( (a, t) : ts ) )
  | isPrefix s a = getIndices (Node [(a, t)])
  | isPrefix a s = findSubstrings' (removePrefix a s) t
  | otherwise    = findSubstrings' s (Node ts)

------------------------------------------------------

sharedPrefix :: String -> String -> String
sharedPrefix "" _
  = ""
sharedPrefix _ ""
  = ""
sharedPrefix (a:as) (b:bs)
  | a == b    = a : sharedPrefix as bs
  | otherwise = ""

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) ( Node ( (a, t) : ts ) )
  -- = undefined
  | p == ""   = Node ( (a, t) : ts' )
  | a == p    = Node ( (a, insert (removePrefix p s, n) t) : ts)
  | otherwise = Node ( (p, Node [(removePrefix p s, Leaf n), (removePrefix p a, t)]) : ts)
  where
    p = sharedPrefix s a
    Node ts' = insert (s, n) (Node ts)

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
