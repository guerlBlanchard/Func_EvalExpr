module Main where

import System.Environment
import System.Exit
import EvalExpr

main :: IO ()
main = do
    args <- getArgs
    evalExpr (head args)
    exitWith (ExitSuccess)