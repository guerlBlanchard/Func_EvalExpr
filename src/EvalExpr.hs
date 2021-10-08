--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- EvalExpr
--

module EvalExpr
    (
    )where

import Parse

eval:: String -> Int 
eval = add
    where
        add = parseOr (mul) (parseOr (mul + add) (mul - add))
        mul = parseOr (pow) (parseOr (pow * pow) (pow / pow))
        pow = parseOr (num) (num ^ pow)
        num = parseOr (dig) (add)
        dig = parseInt 

evalExpr :: String -> Maybe Int
evalExpr = eval