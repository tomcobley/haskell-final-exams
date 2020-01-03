module Types where

type Id = String

data Formula = Var Id
             | Not Formula
             | And Formula Formula
             | Or  Formula Formula
             deriving (Eq, Show)

type NNF = Formula

type CNF = Formula

type CNFRep = [[Int]]

type IdMap = [(Id, Int)]

