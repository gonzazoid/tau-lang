module TauPP(
getModule
) where

import System.Environment
import System.FilePath.Posix
import System.Exit

import Data.List
import qualified Data.Map as Map

import TauParser
import TauExec
import TauSerializer


getModule name = (readFile (getPath name)) >>= (\content -> process $ parse content)
    where getPath name = (map repl name) ++ ".tau"
            where repl '.' = pathSeparator
                  repl c = c

getModules names = sequence $ map getModule names

process ast = do
    let (imports, exports, orderedExports) = getSymbols ast
    importsAst <- getModules (getNames imports)
    return (resolveExports exports orderedExports (makeGlobalScope (zip (getKeys imports) importsAst)))

makeGlobalScope list = foldl extendScope Map.empty list

extendScope scope (keys, importsAst) = foldl (extendScope' importsAst) scope keys
    where extendScope' moduleAst scope (name, alias) = extendScope'' (Map.lookup name moduleAst) alias scope
            where extendScope'' (Just x) alias scope = Map.insert alias x scope

resolveExports  exports orderedExports scope = snd (resolveExports' exports orderedExports scope)
resolveExports' exports orderedExports scope = foldl resolveExport (scope, exports) orderedExports
    where resolveExport (imports, exports) key = resolveExport' key (Map.lookup key exports) (imports, exports)
              where  resolveExport' key (Just (APPLY func args)) (imports, exports) = ((Map.insert key resolvedApply imports), (Map.insert key resolvedApply exports))
                         where resolvedApply = APPLY (resolve imports func) (map (resolve imports) args)
                     resolveExport' key (Just val) (imports, exports) = ((Map.insert key resolvedVal imports), (Map.insert key resolvedVal exports))
                         where resolvedVal = resolve imports val

getNames imports = map (\(name, _) -> name) imports
getKeys  imports = map (\(_, keys) -> keys) imports

getSymbols ast = reverseExportList (getSymbols' ast)
    where reverseExportList (imports, exports, orderedExports)
              | onlyUniqueIds orderedExports = (imports, exports, reverse orderedExports)
                where onlyUniqueIds [] = True
                      onlyUniqueIds (x:xs) = (x `notElem` xs) && (onlyUniqueIds xs)
          getSymbols' (APPLY func args) = process' (func:args) ([], Map.empty, [])
          getSymbols' ast = ([], Map.fromList [("main", ast)], ["main"])

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

processExport ((ID name):(ID "as"):body) rest (imports, exports, orderedExports) = process' rest (imports, (Map.insert name (transform body) exports), (name:orderedExports))
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

processImport' symbols path rest (imports, exports, orderedExports) = process' rest ((path, symbols):imports, exports, orderedExports)

processMain (func:args) (imports, exports, orderedExports) = (imports, (Map.insert "main" (APPLY func args) exports), ("main":orderedExports))
