module TauSerialiser(
serialise
) where

import Data.List 
import TauParser

serialise ast = serialise' ast

serialise' (ID name)  = name
serialise' (FCOMP (ARGS args) body) = "(" ++ (intercalate " " args) ++ " => " ++ (serialise' body) ++ ")"
serialise' (FDEF (ARGS args) (MATCH match) clauses) = "(" ++ (intercalate " " args) ++ " => match " ++ match ++ " " ++ serialisedClauses ++ ")"
    where serialisedClauses = intercalate " " (map serialiseClause clauses)
            where serialiseClause (CLAUSE (MATCH match) (ARGS args) body) = "| " ++ match ++ " " ++ (intercalate " " args) ++ " => " ++ serialise' body
serialise' (APPLY func args) = "(" ++ (serialise' func) ++ " " ++ (intercalate " " (map serialise' args)) ++ ")"
