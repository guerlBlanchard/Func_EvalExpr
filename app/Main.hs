module Main where

import System.Environment
import System.Exit
import EvalExpr

main :: IO ()
main = do
    args <- getArgs
    errorHandling $ length args
    evalExpr (head args)
    exitWith (ExitSuccess)

errorHandling :: Int  -> IO ()
errorHandling 1 = return()
errorHandling _ = exitWith(ExitFailure 84)