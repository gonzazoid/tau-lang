import System.Environment
import System.Exit

import qualified TauParser as Parser(parse)
import TauExec
import TauSerializer

main = getArgs >>= parse >>= putStr . tau

tau  = (++ "\n") . serialize . exec . Parser.parse

parse []     = usage   >> exit
parse ["-v"] = version >> exit
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tau [-v] [file ..]"
version = putStrLn "tau-lang 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
