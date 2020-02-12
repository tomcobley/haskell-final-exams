module Examples where

import Types

--
-- Figure 2 (left)
--
figure :: Trie
figure
  = Node 16960 [Term 1830,
                SubTrie (Node 8208 [Term 73,
                                    SubTrie (Leaf [729,2521])]),
                Term 206]

--
-- Figure 2 (right)
--
figureHashed :: Trie
figureHashed
  = Node 16481 [Term 2521,
                Term 206,
                Term 729,
                SubTrie (Node 48 [Term 73,
                                  Term 1830])]

--
-- Test tries for insert from the spec
--
insT1, insT2, insT3, insT4 :: Trie
insT1
  = Node 512 [Term 2521]
insT2
  = Node 576 [Term 1830,
              Term 2521]
insT3
  = Node 576 [Term 1830,
              SubTrie (Node 8192 [SubTrie (Leaf [729,2521])])]

insT4
  = Node 97 [Term 2521,
             Term 206,
             Term 729]

--
-- Test tries for build from the spec
--
buildT1, buildT2 :: Trie
buildT1
  = Node 14 [Term 1,
             Term 2,
             Term 3]
buildT2
  = Node 1 [SubTrie (Node 1 [SubTrie (Leaf [256,512,768,
                                            1024,1280])])]
