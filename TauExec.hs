module TauExec(
exec
) where

import Debug.Trace
import qualified Data.Map as Map
import TauParser

exec ast = resolve Map.empty ast

resolve scope (ID name) = case Map.lookup name scope of { Nothing -> ID name; Just x -> x}

resolve _ func@(FDEF _ _ _) = func
resolve _ func@(FCOMP _ _)  = func
resolve scope (APPLY func params) = run (resolve scope func) (map (resolve scope) params)

getKeys (FDEF (ARGS args) _ _ ) = args
getKeys (FCOMP (ARGS args) _  ) = args

-- run func params | trace ("\nrun\n\tfunc: " ++ show func ++ "\n\tparams: " ++ show params) False = undefined
run func@(ID _ ) params = APPLY func params

run func params
    | isPartialApply keys params = FCOMP (ARGS reducedArgs) reducedFunc
    | isFullApply    keys params = apply func scope
    where isPartialApply keys params = ((ID "_") `elem` params) || ((length params) < (length keys))
          isFullApply    keys params = length params == length keys
          keys = getKeys func
          scope = makeScope keys params
          apply (FCOMP _ body) scope = resolve scope body
          apply (FDEF (ARGS args) (MATCH id) clauses) scope = resolveClause matchValue (getClause matchValue clauses)
              where
                    matchValue = resolve scope (ID id)
                    getClause (ID name)                clauses = getClause' name clauses 0
                    getClause (APPLY (ID name) params) clauses = getClause' name clauses (length params)
                    resolveClause cgt@(ID _) (CLAUSE (MATCH match) _ body)     = resolve (extendScope scope match [] [] cgt) body
                    resolveClause (APPLY cgt matchValues) (CLAUSE (MATCH match) (ARGS matchNames) body) = resolve (extendScope scope match matchNames matchValues cgt) body
          reducedArgs = filter ((\scope key -> Map.notMember key scope) scope) keys
          reducedFunc = APPLY func resolvedParams
              where resolvedParams = map resolver (zip keys extendedParams) 
                      where extendedParams = params ++ (take ((length keys) - (length params)) (repeat (ID "_")))
                            resolver (key, (ID "_")) = ID key
                            resolver (_, param) = param

getClause' name clauses arity = head $ filterClausesByName name $ filterClausesByArity arity clauses
    where filterClausesByArity arity clauses = filter ((\arity (CLAUSE _ (ARGS args) _ ) -> (length args) == arity) arity) clauses
          filterClausesByName  name  clauses = filter ((\name  (CLAUSE (MATCH x)  _  _ ) -> name == x || x == "_")  name ) clauses

extendScope scope match keys values cgt
    | match == "_"  = Map.union (Map.fromList [("_", cgt)]) extendedScope
    | otherwise     = extendedScope
    where extendedScope = Map.union (makeScope keys values) scope

makeScope keys params = Map.fromList $ filter (\(_, val) -> val /= ID "_" ) (zip keys params)
