module TauExec(
exec, resolve, makeScope
) where

import Debug.Trace
import qualified Data.Map as Map
import TauParser

exec ast = resolve Map.empty ast

resolve scope cgt@(ID name) = resolve' cgt (Map.lookup name scope)
    where resolve' cgt (Nothing) = cgt
          resolve' _ (Just x)  = x

resolve _ func@(FDEF _ _ _) = func
resolve _ func@(FCOMP _ _)  = func
resolve scope (APPLY func params) = run (resolve scope func) (map (resolve scope) params)

-- run func params | trace ("\nrun\n\tfunc: " ++ show func ++ "\n\tparams: " ++ show params) False = undefined
run func@(ID _ ) params = APPLY func params

run func params
    | isPartialApply keys params = FCOMP (ARGS reducedArgs) reducedFunc
    | isFullApply    keys params = apply func scope
    where isPartialApply keys params = ((ID "_") `elem` params) || ((length params) < (length keys))
          isFullApply    keys params = length params == length keys
          keys = getKeys func
              where getKeys (FDEF (ARGS args) _ _ ) = getKeys' args
                    getKeys (FCOMP (ARGS args) _  ) = getKeys' args
                    getKeys' args
                      | onlyUniqueIds args = args
                        where onlyUniqueIds [] = True
                              onlyUniqueIds (x:xs) = (x `notElem` xs) && (onlyUniqueIds xs)
          scope = makeScope keys params
          apply (FCOMP _ body) scope = resolve scope body
          apply (FDEF (ARGS args) (MATCH id) clauses) scope
              | id `elem` args = resolveClause matchValue (getClause matchValue clauses)
              where
                    matchValue = resolve scope (ID id)
                    getClause (ID name)                clauses = getClause' name clauses 0
                    getClause (APPLY (ID name) params) clauses = getClause' name clauses (length params)
                    getClause' name clauses arity = filterClausesByName name $ filterClausesByArity arity clauses
                        where filterClausesByArity arity clauses = filter ((\arity (CLAUSE _ (ARGS args) _ ) -> (length args) == arity) arity) clauses
                              filterClausesByName  name  clauses = filter ((\name  (CLAUSE (MATCH x)  _  _ ) -> name == x || x == "_")  name ) clauses
                    resolveClause cgt@(ID _) ([(CLAUSE (MATCH match) _ body)])     = resolve (extendScope scope match [] [] cgt) body
                    resolveClause (APPLY cgt matchValues) ([(CLAUSE (MATCH match) (ARGS matchNames) body)]) = resolve (extendScope scope match matchNames matchValues cgt) body
                    extendScope scope match keys values cgt
                        | match == "_"  = Map.union (Map.fromList [("_", cgt)]) extendedScope
                        | otherwise     = extendedScope
                        where extendedScope
                                | (Map.intersection extension scope) ==  Map.empty = Map.union extension scope
                                    where extension = makeScope keys values
          reducedArgs = filter ((\scope key -> Map.notMember key scope) scope) keys
          reducedFunc = APPLY func resolvedParams
              where resolvedParams = map resolver (zip keys extendedParams) 
                      where extendedParams = params ++ (take ((length keys) - (length params)) (repeat (ID "_")))
                            resolver (key, (ID "_")) = ID key
                            resolver (_, param) = param

makeScope keys params = Map.fromList $ filter (\(key, val) -> key /= "_" && val /= ID "_" ) (zip keys params)
