module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes n
  | n == 0    = 0 
  | otherwise = (bitTable !! ((bit 4 - 1) .&. n)) + countOnes (shiftR n 4)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i
  = popCount . (.&.) (pred (bit i))

getIndex :: Int -> Int -> Int -> Int
getIndex n blockIndex blockSize
  = (bit blockSize - 1) .&. (shiftR n (blockIndex * blockSize))

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace i (a:as) a'
  | i == 0    = a' : as 
  | otherwise = a : replace (i - 1) as a'

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 a' [] 
  = [a']
insertAt i a' (a:as) 
  | i == 0    = a' : (a : as)
  | otherwise = a : insertAt (i - 1) a' as


--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f g t 
  | Leaf vs         <- t = g vs 
  | Node _ subNodes <- t = sum (map sumSubNode subNodes)
  where 
    sumSubNode :: SubNode -> Int 
    sumSubNode s 
      | Term n    <- s = f n 
      | SubTrie t <- s = sumTrie f g t

--
-- Predefined functions using sumTrie:
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member v hash trie blockSize
  = member' 0 trie
  where 
    member' :: Int -> Trie -> Bool 
    member' currentLevel (Leaf vs)
      = elem v vs
    member' currentLevel (Node bitVector subNodes)
      | isBitSet, Term v'   <- subNode = v == v'
      | isBitSet, SubTrie t <- subNode = member' (currentLevel + 1) t
      | otherwise                      = False
      where 
        i        = getIndex hash currentLevel blockSize
        isBitSet = testBit bitVector i 
        subNode  = subNodes !! (countOnesFrom i bitVector)


--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hashFn maxDepth blockSize v t
  = insert' v (hashFn v) 0 t
  where 
    insert' :: Int -> Int -> Int -> Trie -> Trie
    insert' v hash currentLevel t 
      | (Leaf vs)    <- t            = Leaf (nub (v : vs))
      | currentLevel == maxDepth - 1 = Leaf [v]
    insert' v hash currentLevel (Node bitVector subNodes)
      | bitNotSet  = Node (setBit bitVector i) (insertAt n (Term v) subNodes)
      | SubTrie t' <- s          = Node bitVector (replace' t')     
      | Term v'    <- s, v == v' = t
      | Term v'    <- s          = Node bitVector (replace' 
                                   (insert' v' (hashFn v') (nextLev) empty))
      where
        i          = getIndex hash currentLevel blockSize
        bitNotSet  = not (testBit bitVector i)
        n          = countOnesFrom i bitVector
        s          = subNodes !! n
        nextLev    = currentLevel + 1
        replace' x = replace n subNodes (SubTrie (insert' v hash (nextLev) x))


buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hashFn maxDepth blockSize vs
  = foldl (flip (insert hashFn maxDepth blockSize)) empty vs
