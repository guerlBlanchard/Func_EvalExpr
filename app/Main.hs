module Main where

import System.Environment
import System.Exit
import EvalExpr

main :: IO ()
main = do
    args <- getArgs
    print $ evalExpr (head args)
    exitWith (ExitSuccess)