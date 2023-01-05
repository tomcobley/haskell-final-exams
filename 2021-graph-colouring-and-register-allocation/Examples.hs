module Examples where

import Types

------------------------------------------------------------------
-- Example graphs from the spec...

fig1Left :: Graph Int
fig1Left 
  = ([1,2,3,4],[(1,2),(1,3),(2,4),(3,4)])

fig1Middle:: Graph Int
fig1Middle 
  = ([1,2,3,4],[(1,2),(1,3),(1,4),(2,4),(3,4)])

------------------------------------------------------------------
-- Example expressions...

e1, e2 :: Exp
e1 
  = Apply Add (Var "a") (Var "b")
e2
  = Apply Mul (Apply Add (Var "x") (Const 2)) (Var "y")

------------------------------------------------------------------
-- Example Id map...

idMap1 :: IdMap
idMap1
  = [("a", "a"),("b", "R1"),("y","R1"),("x","R6")]

------------------------------------------------------------------
-- Example functions and associated structures from the spec...

-- Factorial...
fact :: Function
fact
  = ("fact",
     ["n"],
     [If (Apply Eq (Var "n") (Const 0))
         [Assign "return" (Const 1)]
         [Assign "prod" (Const 1),
          Assign "i" (Var "n"),
          While (Apply GEq (Var "i") (Const 1))
                [Assign "prod" (Apply Mul (Var "prod") (Var "i")),
                 Assign "i" (Apply Add (Var "i") (Const (-1)))
                ],
          Assign "return" (Var "prod")
         ]
     ]
    )

factB :: Block
factB 
  = let (_, _, b) = fact in b

factIG :: IG
factIG
  = (["i","n","prod"],[("i","prod"),("n","prod")])

-- Assumes two registers, [1,2]...
factColouring :: Colouring Id
factColouring
  = [("i",2),("n",2),("prod",1)] 

factIdMap :: IdMap
factIdMap
  = [("return","return"),("i","R2"),("n","R2"),("prod","R1")] 

-- Assuming two registers, [1,2]...
factTransformed :: Function
factTransformed
  = ("fact",["n"],
     [Assign "R2" (Var "n"),
      If (Apply Eq (Var "R2") (Const 0))
         [Assign "return" (Const 1)] 
         [Assign "R1" (Const 1),
          While (Apply GEq (Var "R2") (Const 1)) 
                [Assign "R1" (Apply Mul (Var "R1") (Var "R2")),
                 Assign "R2" (Apply Add (Var "R2") (Const (-1)))
                ],
          Assign "return" (Var "R1")
         ]
     ])

factLiveVars :: [[Id]]
factLiveVars
  = [["n"],[],["n"],["n","prod"],["i","prod"],["i","prod"],
     ["i","prod"],["prod"]]

factCFG :: CFG
factCFG
  = [(("_",["n"]),[1,2]),
     (("return",[]),[]),
     (("prod",[]),[3]),
     (("i",["n"]),[4]),
     (("_",["i"]),[5,7]),
     (("prod",["i","prod"]),[6]),
     (("i",["i"]),[4]),
     (("return",["prod"]),[])]

-- Figure 3
fig3
  = ("f", ["a", "n"],
     [Assign "b" (Const 0),
      While (Apply GEq (Var "n") (Const 1))
            [Assign "c" (Apply Add (Var "a") (Var "b")),
             Assign "d" (Apply Add (Var "c") (Var "b")),
             If (Apply GEq (Var "d") (Const 20))
                [Assign "b" (Apply Add (Var "c") (Var "d")),
                 Assign "d" (Apply Add (Var "d") (Const 1))
                ]
                [],
             Assign "a" (Apply Mul (Var "c") (Var "d")),
             Assign "n" (Apply Add (Var "n") (Const (-1)))
            ],
      Assign "return" (Var "d")
     ])

fig3B :: Block
fig3B
  = let (_, _, b) = fig3 in b

fig3IG
  = (["a","b","c","d","n"],
     [("a","b"),("a","d"),("a","n"),
      ("b","c"),("b","d"),("b","n"),
      ("c","d"),("c","n"),
      ("d","n")])

-- Assuming three registers, [1,2,3]...
fig3Colouring :: Colouring Id
fig3Colouring
  = [("a",3),("b",0),("c",3),("d",2),("n",1)]

fig3IdMap :: IdMap
fig3IdMap
  = [("return","return"),("a","R3"),("b","b"),("c","R3"),("d","R2"),("n","R1")]

fig3Transformed :: Function
fig3Transformed
  = ("f",["a","n"],
     [Assign "R3" (Var "a"),
      Assign "R1" (Var "n"),
      Assign "b" (Const 0),
      While (Apply GEq (Var "R1") (Const 1)) 
            [Assign "R3" (Apply Add (Var "R3") (Var "b")),
             Assign "R2" (Apply Add (Var "R3") (Var "b")),
             If (Apply GEq (Var "R2") (Const 20)) 
                [Assign "b" (Apply Add (Var "R3") (Var "R2")),
                 Assign "R2" (Apply Add (Var "R2") (Const 1))
                ] 
                [],
             Assign "R3" (Apply Mul (Var "R3") (Var "R2")),
             Assign "R1" (Apply Add (Var "R1") (Const (-1)))
            ],
      Assign "return" (Var "R2")
      ]
     )

fig3LiveVars :: [[Id]]
fig3LiveVars
  = [["a","d","n"],["a","b","d","n"],["a","b","n"],["b","c","n"],
     ["b","c","d","n"],["c","d","n"],["b","c","d","n"],
     ["b","c","d","n"],["a","b","d","n"],["d"]]

fig3CFG :: CFG
fig3CFG
  = [(("b",[]),[1]),
     (("_",["n"]),[2,9]),
     (("c",["a","b"]),[3]),
     (("d",["b","c"]),[4]),
     (("_",["d"]),[5,7]),
     (("b",["c","d"]),[6]),
     (("d",["d"]),[7]),
     (("a",["c","d"]),[8]),
     (("n",["n"]),[1]),
     (("return",["d"]),[])]



