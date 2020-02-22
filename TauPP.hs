module TauPP(
getModule
) where

import System.Environment
import System.Exit

import Data.List
import qualified Data.Map as Map

import TauParser
import TauExec
import TauSerializer


getModule name = (readFile name) >>= (\content -> process $ parse content)

getModules names = sequence $ map getModule names

process ast = do
    let (imports, exports) = getSymbols ast
    importsAst <- getModules (getNames imports)
    return (resolveExports exports (makeGlobalScope (zip (getKeys imports) importsAst)))

makeGlobalScope list = foldl extendScope Map.empty list

extendScope scope (keys, importsAst) = foldl (extendScope' importsAst) scope keys
extendScope' moduleAst scope (name, alias) = case Map.lookup name moduleAst of Just x -> Map.insert alias x scope
                                                                               Nothing -> scope -- we have to die here

resolveExports exports scope = Map.map (resolveExport scope) exports
    where resolveExport scope (APPLY func args) = APPLY (resolve scope func) (map (resolve scope) args)
          resolveExport scope export = resolve scope export

getNames imports = map (\(name, _) -> name) imports
getKeys  imports = map (\(_, keys) -> keys) imports

getSymbols (APPLY func args) = process' (func:args) ([], Map.empty)
getSymbols ast = ([], Map.fromList [("main", ast)])

process' [] scope = scope
process' ((ID "export"):rest) scope = processExport rest [] scope
process' (x:xs) scope
  | isImport x = processImport (getBody x) xs scope
  | isExport x = processExport (getBody x) xs scope
  | otherwise  = processMain (x:xs) scope
  where isImport (APPLY (ID "import") _ ) = True
        isImport _ = False

        isExport (APPLY (ID "export") _ ) = True
        isExport _ = False

        getBody (APPLY _ args) = args

processExport ((ID name):(ID "as"):body) rest (imports, exports) = process' rest (imports, (Map.insert name (transform body) exports))
    where transform (x:[]) = x
          transform (func:body) = APPLY func body

-- (import name from path)
processImport ((ID name):(ID "from"):(ID path):[])                      rest scope = processImport' [(name, name)] path rest scope

-- (import name as name from path)
processImport ((ID name):(ID "as"):(ID alias):(ID "from"):(ID path):[]) rest scope = processImport' [(name, alias)] path rest scope

-- (import (names) from path)
-- (import ((name as name) ...) from path
processImport ((APPLY func args):(ID "from"):(ID path):[])              rest scope = processImport' (getImportSymbols (func:args)) path rest scope
    where getImportSymbols symbols = map getImportSymbol symbols
            where getImportSymbol (ID name) = (name, name)
                  getImportSymbol (APPLY (ID name) [(ID "as"), (ID alias)]) = (name, alias)

processImport' symbols path rest (imports, exports) = process' rest ((path, symbols):imports, exports)

processMain (func:args) (imports, exports) = (imports, (Map.insert "main" (APPLY func args) exports))
