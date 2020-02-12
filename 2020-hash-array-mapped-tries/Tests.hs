module Tests where

import IC.TestSuite
import Tries
import Examples


-- countOnes 0 == 0
-- countOnes 65219 == 11
countOnesTestCases
  = [ 0 ==> 0,
      65219 ==> 11
    ]


-- countOnesFrom 0 88 == 0
-- countOnesFrom 3 15 == 3
countOnesFromTestCases
  = [ (0,88) ==> 0,
      (3,15) ==> 3
    ]


-- getIndex 53767 0 4 == 7
-- getIndex 53767 3 4 == 13
-- getIndex 53767 1 8 == 210
getIndexTestCases
  = [ (53767,0,4) ==> 7,
      (53767,3,4) ==> 13,
      (53767,1,8) ==> 210
    ]


-- replace 0 "trie" 'b' == "brie"
-- replace 3 "trie" 'p' == "trip"
replaceTestCases
  = [ (0,"trie",'b') ==> "brie",
      (3,"trie",'p') ==> "trip"
    ]


-- insertAt 3 'p' "trie" == "tripe"
-- insertAt 4 's' "trie" == "tries"
insertAtTestCases
  = [ (3,'p',"trie") ==> "tripe",
      (4,'s',"trie") ==> "tries"
    ]


-- No autotests for sumTrie, but you can try the following manually:
-- sumTrie (const 1) length empty == 0
-- sumTrie (const 1) (const 1) figure == 4


-- No autotests for member, but you can try the following manually:
-- member 12 12 figure 4 == False
-- member 73 73 figure 4 == True
-- member 206 (hash 206) figureHashed 4 == True


-- No autotests for insert, but you can try the following manually:
-- insert id 3 4 2521 empty == insT1
-- insert id 3 4 1830 (insert id 3 4 2521 empty) == insT2
-- insert id 3 4 729 (insert id 3 4 1830 (insert id 3 4 2521 empty)) == insT3
-- insert hash 3 4 206 (insert hash 3 4 2521 (insert hash 3 4 729 empty)) == insT4


-- No autotests for buildTrie, but you can try the following manually:
-- buildTrie id 3 4 [1..3] == buildT1
-- buildTrie id 3 4 [n * 256 | n <- [1..5]] == buildT2
-- buildTrie id 3 4 [73,206,729,1830,2521] == figure
-- buildTrie hash 3 4 [73,206,729,1830,2521] == figureHashed


-- You can add your own test cases above

allTestCases
  = [
      TestCase "countOnes" (countOnes)
              countOnesTestCases
    , TestCase "countOnesFrom" (uncurry countOnesFrom)
              countOnesFromTestCases
    , TestCase "getIndex" (uncurry3 getIndex)
              getIndexTestCases
    , TestCase "replace" (uncurry3 replace)
              replaceTestCases
    , TestCase "insertAt" (uncurry3 insertAt)
              insertAtTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
