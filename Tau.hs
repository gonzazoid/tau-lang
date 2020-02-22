import System.Environment
import System.Exit

import qualified Data.Map as Map

import TauExec
import TauSerializer
import TauPP

main = getArgs >>= parse >>= putStr . tau

tau  = (++ "\n") . serialize . exec

parse []     = usage   >> exit
parse ["-v"] = version >> exit
parse ([fs])     = run fs

usage   = putStrLn "Usage: tau [-v] [file ..]"
version = putStrLn "tau-lang 0.1"
run fs = do
    ast <- getModule fs
    case Map.lookup "main" ast of Just x -> return x
                                  Nothing -> Main.die

exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
