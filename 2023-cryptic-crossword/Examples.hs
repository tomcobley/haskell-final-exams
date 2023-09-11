module Examples where

import Types
import Clues

-- Parses whose parse trees correspond to Figure 1
figParses :: [Parse]
figParses
  = [(["new"],[],Reversal ["revolves"] (Synonym "toaster")),
     (["new"],[],Charade [] (Synonym "toaster") (Synonym "revolves")),
     (["revolves"],[],Anagram ["new"] "toaster"),
     (["revolves"],[],Charade [] (Synonym "new") (Synonym "toaster"))]

-- The 'correct' parse trees and parses for the first 19 clues.
trees :: [ParseTree]
trees
  = [Synonym "worry",
     Anagram ["new"] "toaster",
     Anagram ["unusual"] "hensdark",
     Anagram ["meandering"] "stream",
     Reversal ["from","east","to","west"] (Synonym "made"),
     Reversal ["returning"] (Synonym "nappy"),
     Insertion ["ringing"] (Synonym "home") (Synonym "girl"),
     Insertion ["about"] (Synonym "a") (Synonym "hint"),
     Insertion ["inhabited","by"] (Synonym "american") (Synonym "spot"),
     Insertion ["featured","in"] (Synonym "one") (Synonym "superstitions"),
     Charade [] (Synonym "taxi") (Synonym "home"),
     Charade ["needing"] (Synonym "son") (Synonym "diaper"),
     Charade ["on"] (Insertion ["about"] (Synonym "chap") (Synonym "worry")) 
                    (Synonym "the french"),
     Insertion ["in"] (Charade [] (Synonym "a") (Synonym "street")) 
                      (Synonym "new york"),
     Charade ["by"] (Reversal ["turning"] (Synonym "or")) (Synonym "gallery"),
     Charade [] (Synonym "at home") (Anagram ["ornate"] "tap"),
     Insertion ["in"] (Synonym "carol") (Charade [] (Synonym "county") 
                      (Synonym "working")),
     Charade [] (Reversal ["up"] (Synonym "fed")) 
                (Charade [] (Synonym "one") (Synonym "worker")),
     Reversal ["about"] (Charade [] (Synonym "deep") (Synonym "south"))]

parses :: [Parse]
parses
  = zipWith (\(def, link) t -> (def, link, t))
            [(["business"], []),
             (["revolves"], []),
             (["bird"], []),
             (["perfect"], []),
             (["cheese"], []),
             (["recompensed"], ["for"]),
             (["pardon"], []),
             (["rest"], []),
             (["bear"], []),
             (["river"], []),
             (["hut"], ["from"]),
             (["irritable"], []),
             (["port"], []),
             (["filthy"], ["thats"]),
             (["turn"], []),
             (["unsuitable"], []),
             (["palace"], ["for"]),
             (["uncooperative"], ["becomes"]),
             (["race"], [])]
            trees

enumerations :: [Int]
enumerations  
  = [7,7,8,6,4,6,7,9,7,5,5,6,9,5,6,5,10,7,5,6,4,5,4]

-- For the first 19 clues the first element should match 
-- the corresponding tree in trees above; the second should not.
-- The rest are used to test Part IV.
strings :: [(String, String)]
strings
  = [("concern","mystery"),
     ("rotates","serator"),
     ("redshank","redstart"),
     ("master","matter"),
     ("edam","mead"),
     ("repaid","repair"),
     ("amnesty","portent"),
     ("remainder","sacrament"),
     ("sustain","extorts"),
     ("loire","stour"),
     ("cabin","manic"),
     ("snappy","strain"),
     ("fremantle","fantastic"),
     ("nasty","conch"),
     ("rotate","traits"),
     ("inapt","mound"),
     ("kensington","acceptance"),
     ("defiant","spender"),
     ("speed","peeds"),
     ("ling","parr"),
     ("edward","robert"),
     ("hera","thor"),
     ("large","small"),
     ("tofu","lard")]


