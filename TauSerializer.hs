module TauSerializer(
serialize
) where

import Data.List 
import TauParser

serialize ast = serialize' ast

serialize' (ID name)  = name
serialize' (FCOMP (ARGS args) body) = "(" ++ (intercalate " " args) ++ " => " ++ (serialize' body) ++ ")"
serialize' (FDEF (ARGS args) (MATCH match) clauses) = "(" ++ (intercalate " " args) ++ " => match " ++ match ++ " " ++ serialisedClauses ++ ")"
    where serialisedClauses = intercalate " " (map serialiseClause clauses)
            where serialiseClause (CLAUSE (MATCH match) (ARGS args) body) = "| " ++ match ++ " " ++ (intercalate " " args) ++ " => " ++ serialize' body
serialize' (APPLY func args) = "(" ++ (serialize' func) ++ " " ++ (intercalate " " (map serialize' args)) ++ ")"
