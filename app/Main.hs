module Main where

import System.Environment
import System.Exit
import EvalExpr

main :: IO ()
main = do
    args <- getArgs
    evalExpr args[1]
    exitWith (ExitSuccess)
