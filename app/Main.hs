module Main where

import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs

    exitWith (ExitSuccess)
