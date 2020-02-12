{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Data.Bits

type BitVector = Int

data Trie = Leaf [Int] | Node BitVector [SubNode]
          deriving (Eq, Show)

data SubNode = Term Int | SubTrie Trie
             deriving (Eq, Show)

type Hash = Int

type HashFun = Int -> Hash

empty :: Trie 
empty
  = Node 0 []

-----------------------------------------------------------------
-- Show function for trees

-- Only needed for displaying bit vectors...
maxDegree :: Int
maxDegree
  = 16

showBitVector :: Int -> Int -> String
showBitVector bv 0
  = ""
showBitVector bv n
  = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

showT :: Trie -> IO ()
showT t
  = showT' t 0

showT' :: Trie -> Int -> IO ()
showT' (Leaf vs) indent
  = do
      putStr (replicate indent ' ')
      putStrLn ("  " ++ show vs)
showT' (Node bv ts) indent
  = do
      putStrLn (replicate indent ' ' ++ showBitVector bv maxDegree)
      mapM_ (flip showT'' (indent + 2)) ts

showT'' (Term v) indent
  = putStrLn (replicate indent ' ' ++ "<" ++ show v ++ ">")
showT'' (SubTrie t) indent
  = showT' t indent

